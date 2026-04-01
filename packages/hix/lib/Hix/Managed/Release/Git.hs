module Hix.Managed.Release.Git where

import Data.IORef (IORef, readIORef)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Exon (exon)
import System.Posix (epochTime)

import Hix.Class.Map (nElems, nForWithKey, nOver, nOverWithKey, nSize, nZip)
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.Version (Version)
import Hix.Managed.Data.CreatedRefs (CreatedRefs (..), accumulateRefs, newRefsRef)
import Hix.Managed.Data.Packages (Packages (..))
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import qualified Hix.Managed.Git as Git
import Hix.Managed.Git (BranchName (..), GitApi (..), GitNative (..), gitApiHermeticM, gitApiM)
import Hix.Managed.Git.Native (
  BracketResult (..),
  GitBracketConfig (..),
  createBranch,
  noBracketResult,
  wrapBracketWithRefs,
  )
import Hix.Maybe (justIf)
import Hix.Pretty (showP)

data CommitOrTag =
  CommitCmd
  |
  TagCmd
  deriving stock (Eq, Show)

data CommitStyle =
  CommitIndividual
  |
  CommitShared Version
  deriving stock (Eq, Show)

data GitExtraArgs =
  GitExtraArgs {
    commitArgs :: [Text],
    tagArgs :: [Text]
  }
  deriving stock (Eq, Show, Generic)

defaultGitExtraArgs :: GitExtraArgs
defaultGitExtraArgs = GitExtraArgs {commitArgs = [], tagArgs = []}

-- | Result of the commit step, containing info about what was created.
data CommitResult =
  CommitResult {
    -- | Description of the commit (package names or shared version).
    names :: Maybe Text,
    -- | Tags that were created.
    tags :: Set Git.Tag
  }
  deriving stock (Eq, Show, Generic)

noCommitResult :: CommitResult
noCommitResult = CommitResult {names = Nothing, tags = Set.empty}

data GitRelease =
  GitRelease {
    bracket :: forall a . M a -> M (BracketResult, Maybe BranchName, a),
    commit :: Packages Version -> CommitStyle -> M CommitResult
  }

messageComponents :: Packages Version -> CommitStyle -> (Text, Packages Text)
messageComponents packages style =
  (desc, trailers)
  where
    desc = case style of
      CommitIndividual ->
        if count <= 4
        then Text.intercalate ", " (nElems pids)
        else [exon|#{show count} packages|]
      CommitShared version ->
        showP version

    count = nSize packages

    trailers :: Packages Text
    trailers = nOver pids \ pid -> [exon|release-id:#{pid}|]

    pids :: Packages Text
    pids = nOverWithKey packages \ (LocalPackage name) version -> showP PackageId {..}

commitOrTag ::
  GitNative ->
  CommitOrTag ->
  [Text] ->
  [Text] ->
  Text ->
  [Text] ->
  M ()
commitOrTag git cmd extraArgs args desc trailers =
  git.cmd_ ([cmdText, "-m", [exon|Release #{desc}|]] ++ concat [["-m", t] | t <- trailers] ++ args ++ extraArgs)
  where
    cmdText = case cmd of
      CommitCmd -> "commit"
      TagCmd -> "tag"

createTag ::
  GitNative ->
  [Text] ->
  Maybe LocalPackage ->
  Version ->
  [Text] ->
  M Git.Tag
createTag git extraArgs package version trailers = do
  let tag = Git.Tag {package, version}
  commitOrTag git TagCmd extraArgs [showP tag] (showP tag) trailers
  pure tag

createTags ::
  GitNative ->
  [Text] ->
  Packages Version ->
  Packages Text ->
  CommitStyle ->
  M CreatedRefs
createTags git extraArgs packages trailers = \case
  CommitIndividual -> do
    let zipped = nZip (,) packages trailers
    tags <- nForWithKey @(Packages _) zipped createPkgTag
    pure CreatedRefs {branches = [], tags = Set.fromList (nElems @(Packages _) tags)}
  CommitShared version -> do
    tag <- createTag git extraArgs Nothing version (nElems trailers)
    pure CreatedRefs {branches = [], tags = [tag]}
  where
    createPkgTag package (version, trailer) =
      createTag git extraArgs (Just package) version [trailer]

-- | Create a release branch if it doesn't exist yet.
-- Checks the 'CreatedRefs' to see if the branch was already created.
ensureReleaseBranch ::
  GitNative ->
  IORef CreatedRefs ->
  BranchName ->
  M ()
ensureReleaseBranch git refsRef branch = do
  refs <- liftIO (readIORef refsRef)
  unless (Set.member branch refs.branches) do
    createBranch git refsRef branch Nothing

-- | Check if a branch was created by looking at 'CreatedRefs'.
branchCreated ::
  IORef CreatedRefs ->
  BranchName ->
  M Bool
branchCreated refsRef branch = do
  refs <- liftIO (readIORef refsRef)
  pure (Set.member branch refs.branches)

-- | Bracket a release action with git operations.
-- Branch creation is deferred until a commit is made.
-- Returns a 'BracketResult' and the release branch name (if created).
withReleaseBranch ::
  forall a .
  ReleaseConfig ->
  GitNative ->
  IORef CreatedRefs ->
  BranchName ->
  M a ->
  M (BracketResult, Maybe BranchName, a)
withReleaseBranch config git refs branch action = do
  (bracketResult, (releaseBranch, result)) <-
    wrapBracketWithRefs git bracketConfig refs do
      result <- action
      created <- branchCreated refs branch
      pure (justIf created branch, result)
  pure (bracketResult, releaseBranch, result)
  where
    bracketConfig = GitBracketConfig {
      push = config.push,
      fetch = False,
      merge = config.merge
    }

gitReleaseNative :: ReleaseConfig -> GitExtraArgs -> GitNative -> M GitRelease
gitReleaseNative config extraArgs git = do
  refsRef <- newRefsRef
  epoch <- show <$> liftIO epochTime
  let branch = BranchName [exon|release-prepare-#{epoch}|]
  pure GitRelease {
    bracket = withReleaseBranch config git refsRef branch,
    commit = commitImpl refsRef branch
  }
  where
    commitImpl refsRef branch packages style = do
      let (names, trailers) = messageComponents packages style
      committedNames <- if config.commit
        then do
          ensureReleaseBranch git refsRef branch
          git.cmd_ ["add", "--all"]
          commitOrTag git CommitCmd extraArgs.commitArgs ["--allow-empty"] names (nElems trailers)
          pure (Just names)
        else pure Nothing
      tagRefs <- if config.tag
        then do
          refs <- createTags git extraArgs.tagArgs packages trailers style
          accumulateRefs refsRef refs
          pure refs.tags
        else pure Set.empty
      pure CommitResult {names = committedNames, tags = tagRefs}

gitApiReleaseProd :: ReleaseConfig -> GitExtraArgs -> GitApi GitRelease
gitApiReleaseProd config extraArgs = gitApiM (gitReleaseNative config extraArgs)

gitApiReleaseHermetic :: ReleaseConfig -> GitExtraArgs -> GitApi GitRelease
gitApiReleaseHermetic config extraArgs = gitApiHermeticM (gitReleaseNative config extraArgs)

gitReleaseUnitTest :: GitRelease
gitReleaseUnitTest =
  GitRelease {
    bracket = \ action -> do
      result <- action
      pure (noBracketResult, Just "release", result)
    ,
    commit = \ _ _ -> pure noCommitResult
  }

gitApiReleaseUnitTest :: GitApi GitRelease
gitApiReleaseUnitTest = GitApi (\_ use -> use gitReleaseUnitTest)

