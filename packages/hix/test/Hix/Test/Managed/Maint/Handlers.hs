module Hix.Test.Managed.Maint.Handlers where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian, timeOfDayToTime)
import Data.Tuple.Extra (fst3)
import Data.Typeable (cast)
import Distribution.Compat.CharParsing (string)
import Distribution.Parsec (CabalParsing, Parsec (parsec), explicitEitherParsec)
import Distribution.Version (version0)
import Exon (exon)
import Path (parseRelDir, parseRelFile)
import Type.Reflection (typeRep)

import Hix.Class.Map (nAdjust, nElems, nKeys, nKeysSet, nMap, nMapWithKey, nOver, nTransform, nViaA, (!?))
import Hix.Data.Dep (Dep (..))
import Hix.Data.EnvName (EnvName)
import Hix.Data.MDep (MDep (..))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M, appRes)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage (..), PackageName)
import Hix.Managed.Cabal.Data.Revision (Revision (..))
import Hix.Managed.Data.BuildOutput (DepChanges (..), ModifiedId (..))
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.MaintContext (MaintContext (..), MaintPackage (..))
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..))
import Hix.Managed.Data.Mutable (depName)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Git (BranchName (..), GitApi (..), MaintBranch (..), Tag (..))
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import Hix.Managed.Handlers.Context (ContextKey (ContextMaint), ContextQuery (ContextQuery))
import Hix.Managed.Handlers.HackageClient (
  HackageClient (..),
  HackageError (..),
  HackageRequest (..),
  HackageResponse (..),
  )
import Hix.Managed.Handlers.Maint (MaintHandlers (..))
import Hix.Managed.Maint.Git (GitMaint (..), formatReleaseBranch, releaseBranchName)
import Hix.Monad (noteFatal)
import Hix.Pretty (HPretty (..), field, prettyMap, showP, showPL)
import Hix.Test.Managed.ReleaseMaintenance.Case (
  EnvStyle (..),
  MaintTestCase (..),
  PackageConf (..),
  PackageMeta (..),
  ProjectHistoryEvent (..),
  TestDep (ModifiedDep),
  partitionTestDeps,
  testDepName,
  )
import Hix.Test.Managed.ReleaseMaintenance.Gen (oldDepBounds, oldDepRange)
import Hix.Test.Managed.Run (addFile)

dummyTime :: UTCTime
dummyTime = UTCTime (fromGregorian 2024 1 16) (timeOfDayToTime (TimeOfDay 0 0 0))

dummyRevision :: Word -> Revision
dummyRevision number = Revision {user = "user-rev", time = dummyTime, sha256 = "deadbeef", number}

data MaintEvent =
  EventFetchRevisions
  |
  EventPublishRevision { packageId :: PackageId, form :: (Text, Text) }
  |
  GitCommitted {
    package :: LocalPackage,
    branch :: MaintBranch,
    modified :: NonEmpty ModifiedId
  }
  deriving stock (Eq, Show)

data GitState =
  GitState {
    branches :: [(MaintBranch, BranchName, Tag)],
    current :: Maybe MaintBranch
  }
  deriving stock (Eq, Show)

data MaintState =
  MaintState {
    events :: [MaintEvent],
    published :: Set PackageName,
    git :: GitState
  }
  deriving stock (Eq, Show, Generic)

pathParserRevisions :: CabalParsing m => m PackageId
pathParserRevisions = do
  string "package/"
  pid <- parsec
  string "/revisions/"
  pure pid

pathParserPublishRevision :: CabalParsing m => m PackageId
pathParserPublishRevision = do
  string "package/"
  pid <- parsec
  string [exon|/#{toString pid.name}.cabal/edit|]
  pure pid

hackage :: ∀ a . Packages PackageMeta -> MVar MaintState -> HackageRequest a -> M (Either HackageError a)
hackage packages state = \case
  HackageRequest {path, accept = HackageResponseJson}
    | Right pid <- explicitEitherParsec pathParserRevisions (toString path)
    , Just package <- packages !? LocalPackage pid.name
    ->
      liftIO $ modifyMVar state \case
        MaintState {events, published, ..} -> do
          let inc = if Set.member pid.name published then 1 else 0
          res <- respond @[_] (dummyRevision <$> [0..package.revision + inc])
          pure (MaintState {events = EventFetchRevisions : events, published, ..}, res)
    where
      respond :: ∀ r . Typeable r => r -> IO (Either HackageError a)
      respond r | Just a <- cast r = pure (Right a)
                | otherwise = pure (Left (HackageFatal [exon|Unexpected json type: #{show (typeRep @a)}|]))

  HackageRequest {path, accept = HackageResponseHtml, body = Just (Left formData)}
    | Right pid <- explicitEitherParsec pathParserPublishRevision (toString path)
    ->
      withState \ MaintState {..} -> do
        let new = MaintState {published = Set.insert pid.name published, ..}
        pure (EventPublishRevision pid (NonEmpty.head formData), new, Right "all good")

  HackageRequest {path, accept} ->
    pure (Left (HackageFatal [exon|Unexpected Hackage request for path '#{path}' with type '#{show accept}'|]))
  where
    withState :: ∀ x . (MaintState -> IO (MaintEvent, MaintState, x)) -> M x
    withState f =
      liftIO $ modifyMVar state \ oldState -> do
        (newEvent, MaintState {..}, a) <- f oldState
        pure (MaintState {events = newEvent : events, ..}, a)

data MaintPremise =
  MaintPremise {
    packages :: Packages PackageMeta,
    tags :: [Tag],
    branches :: [(MaintBranch, BranchName, Tag)]
  }
  deriving stock (Eq, Show)

package0 ::
  EnvStyle ->
  Bool ->
  LocalPackage ->
  PackageConf ->
  PackageMeta
package0 envStyle anyDepModified package PackageConf {..} =
  PackageMeta {
    package,
    deps,
    version = version0,
    modified,
    bumped = any isRangeBump (nElems deps),
    envModified,
    released = False,
    revision = 0,
    shared = False
  }
  where
    envModified = case envStyle of
      EnvAll -> anyDepModified
      EnvEach -> modified

    modified = any isBump (nElems deps)

    isBump = \case
      ModifiedDep _ -> True
      _ -> False

    isRangeBump = \case
      ModifiedDep ModifiedId {range = Just _} -> True
      _ -> False

evolveHistory :: MaintTestCase -> MaintPremise
evolveHistory testCase =
  foldl' step MaintPremise {
    packages = nMapWithKey (package0 testCase.envStyle testCase.anyDepModified) testCase.packages,
    tags = [],
    branches = []
  } testCase.history
  where

    step MaintPremise {..} = \case
      Release {..} ->
        let
          version = bump major ((.version) <$> packages !? package)
          newPackages = nAdjust package packages (release version revision False)
        in withBranches newPackages (Just package) [package] version revision
      ReleaseShared {..} ->
        let
          version = bump major (maximum ((.version) <$> nElems packages))
          newPackages = nMap (release version revision True) packages
        in withBranches newPackages Nothing (nKeys packages) version revision
      where
        withBranches newPackages tagPackage names version revision =
          let
            tag = Tag {package = tagPackage, version}
            newBranch | 0 <- revision = []
                      | otherwise = [(branch tag name version) | name <- names]
          in MaintPremise {packages = newPackages, tags = tag : tags, branches = newBranch ++ branches}

    branch tag name version =
      let mb = MaintBranch {package = name, version}
      in (mb, releaseBranchName mb, tag)

    release version revision shared PackageMeta {package, deps, modified, bumped, envModified} = do
      PackageMeta {version, released = True, ..}

    bump major = \case
      Just [s, ma, mi] -> if major then [s, ma + 1, 0] else [s, ma, mi + 1]
      _ -> if major then [0, 1, 0] else [0, 0, 1]

depChanges :: [PackageMeta] -> DepChanges
depChanges packages =
  DepChanges {
    modified,
    unmodified,
    failed
  }
  where
    (unmodified, modified, failed) = partitionTestDeps allDeps

    allDeps = nElems (mconcat ((.deps) <$> packages))

data EnvOutput =
  EnvOutput {
    targets :: [PackageMeta],
    changes :: DepChanges
  }
  deriving stock (Eq, Show)

instance HPretty EnvOutput where
  hpretty EnvOutput {..} =
    prettyMap "env" [
      field "targets" targets
    ]

buildOutputs :: Packages PackageMeta -> EnvStyle -> Envs EnvOutput
buildOutputs packages = \case
  EnvAll ->
    [("latest", output (nElems packages))]
  EnvEach ->
    flip nTransform packages \ name pkg ->
      ([exon|latest-##{name}|], output [pkg])
  where
    output targets =
      EnvOutput {
        targets,
        changes = depChanges targets
      }

gitMaint ::
  MVar MaintState ->
  MaintPremise ->
  GitMaint
gitMaint state premise =
  GitMaint {..}
  where
    bracket ma = ma

    readTags = pure premise.tags

    listTargetBranches target = do
      branches <- allBranches
      pure (filter ((target ==) . (.package)) branches)

    branchOffTag branch tag =
      update \ GitState {..} ->
        GitState {
          current = Just branch,
          branches = (branch, BranchName (formatReleaseBranch branch), tag) : branches,
          ..
        }

    switchBranch branch = do
      update \ GitState {..} -> GitState {current = Just branch, ..}
      pure (releaseBranchName branch)

    commitBump branch package modified = do
      updateState \ MaintState {..} -> MaintState {events = GitCommitted {package, branch, modified} : events, ..}
      pure (releaseBranchName branch)

    allBranches = liftIO $ readMVar state <&> \ MaintState {git = GitState {branches}} ->
      fst3 <$> branches ++ premise.branches

    update f = updateState \ MaintState {git, ..} -> MaintState {git = f git, ..}

    updateState f = liftIO $ modifyMVar_ state (pure . f)

buildCabalConfig :: PackageMeta -> DepChanges -> M Text
buildCabalConfig PackageMeta {package, deps} DepChanges {..} = do
  pure [exon|cabal-version: 2.2
name: ##{package}
version: 0.2.0
library ##{package}
  build-depends:
    #{showPL bdeps}
|]
  where
    bdeps = sortOn (.package) (mapMaybe mdep modified ++ mapMaybe udep unmodified)

    mdep ModifiedId {package = dpackage, range} =
      let name = depName dpackage
      in if isDep name
      then Just MDep {package = name, bounds = fromMaybe oldDepBounds range}
      else Nothing

    udep d =
      let name = depName d
      in if isDep name
      then Just MDep {package = name, bounds = oldDepBounds}
      else Nothing

    isDep = flip Set.member depNames

    depNames = nKeysSet deps

-- TODO we still need to keep adding the cabal file since publishRevision reads it.
-- It should probably generate it using the flake, on the fly (and this could then be mocked here).
runBump ::
  Envs EnvOutput ->
  EnvName ->
  M DepChanges
runBump outputs env = do
  root <- appRes.root
  EnvOutput {targets, changes} <- noteFatal [exon|No build output in test data for '#{showP env}'|] (outputs !? env)
  for_ targets \ meta@PackageMeta {package} -> do
    cabalPath <- parseRelFile [exon|packages/##{package}/##{package}.cabal|]
    cabalConfig <- buildCabalConfig meta changes
    addFile root cabalPath cabalConfig
  pure changes

testContext :: Envs EnvOutput -> Packages PackageMeta -> M MaintContext
testContext outputs packages = do
  maintPackages <- nViaA (Map.traverseWithKey maintPackage) packages
  pure MaintContext {packages = maintPackages, hackage = [], envs}
  where
    maintPackage package PackageMeta {version, deps} = do
      path <- pathError package $ parseRelDir [exon|packages/##{package}|]
      pure MaintPackage {
        package = ManagedPackage {
          name = package,
          version,
          deps = [Dep {package = testDepName dep, version = oldDepRange} | dep <- nElems deps]
        },
        path
      }

    envs = nOver outputs \ env -> [package | PackageMeta {package} <- env.targets]

    pathError package = noteFatal [exon|testContext: Invalid package name for path: #{showP package}|]

configQuery :: MaintContext -> ContextQuery a -> Maybe a
configQuery conf = \case
  ContextQuery ContextMaint -> Just conf
  _ -> Nothing

maintTestHandlers ::
  Envs EnvOutput ->
  MaintPremise ->
  M (MaintHandlers, MaintContext, MVar MaintState)
maintTestHandlers outputs premise = do
  state <- liftIO $ newMVar initialState
  context <- testContext outputs premise.packages
  let handlers = MaintHandlers {
    runBump = runBump outputs,
    git = GitApi \ _ f -> f (gitMaint state premise),
    context = ContextHandlers.handlersTest (const (pure Nothing)),
    publishHackages = [HackageClient "mock hackage" (hackage premise.packages state)]
  }
  pure (handlers, context, state)
  where
    initialState =
      MaintState {events = [], published = [], git = GitState {branches = [], current = Nothing}}
