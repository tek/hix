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
import Hix.Managed.Git.Native (GitBracketConfig (..), createBranch, wrapBracketWithRefs)
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

data GitRelease =
  GitRelease {
    bracket :: ∀ a . M a -> M (Maybe BranchName, a),
    commit :: Packages Version -> CommitStyle -> M ()
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
-- Returns 'Nothing' if no branch was created (only tags).
withReleaseBranch ::
  ∀ a .
  ReleaseConfig ->
  GitNative ->
  IORef CreatedRefs ->
  BranchName ->
  M a ->
  M (Maybe BranchName, a)
withReleaseBranch config git refs branch action = do
  wrapBracketWithRefs git bracketConfig refs do
    result <- action
    created <- branchCreated refs branch
    pure (if created then Just branch else Nothing, result)
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
      when config.commit do
        ensureReleaseBranch git refsRef branch
        git.cmd_ ["add", "--all"]
        commitOrTag git CommitCmd extraArgs.commitArgs ["--allow-empty"] names (nElems trailers)
      when config.tag do
        refs <- createTags git extraArgs.tagArgs packages trailers style
        accumulateRefs refsRef refs

gitApiReleaseProd :: ReleaseConfig -> GitExtraArgs -> GitApi GitRelease
gitApiReleaseProd config extraArgs = gitApiM (gitReleaseNative config extraArgs)

gitApiReleaseHermetic :: ReleaseConfig -> GitExtraArgs -> GitApi GitRelease
gitApiReleaseHermetic config extraArgs = gitApiHermeticM (gitReleaseNative config extraArgs)

gitReleaseUnitTest :: GitRelease
gitReleaseUnitTest =
  GitRelease {
    bracket = \ action -> do
      result <- action
      pure (Just "release", result)
    ,
    commit = \ _ _ -> pure ()
  }

gitApiReleaseUnitTest :: GitApi GitRelease
gitApiReleaseUnitTest = GitApi (\_ use -> use gitReleaseUnitTest)
