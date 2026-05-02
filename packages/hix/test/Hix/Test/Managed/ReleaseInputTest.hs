module Hix.Test.Managed.ReleaseInputTest where

import GHC.Exts (fromList)
import Hedgehog (Gen, Property, annotateShow, forAll, property, test, withTests, (===))
import qualified Hedgehog.Gen as Gen
import Path (reldir)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Data.Error (Error (..), ErrorMessage (..))
import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.Options (ReleaseOptions (..))
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Managed.Cabal.Config (cabalConfig)
import Hix.Managed.Data.ReleaseConfig (ArtifactConfig (..), ReleaseConfig (..), ReleaseVersion (..))
import Hix.Managed.Data.ReleaseContext (ReleaseContextProto (..), ReleasePackage (..))
import qualified Hix.Managed.Data.VersionIncrement as VersionIncrement
import Hix.Managed.Git (GitNative (..), runGitNativeHermetic)
import qualified Hix.Managed.Handlers.Release.Test as ReleaseTest
import Hix.Managed.Handlers.Release.Test (ReleaseTestConfig (..))
import Hix.Managed.Release (release)
import Hix.Managed.Release.Data.Staged (ReleaseState (..), Termination (..))
import Hix.Managed.Release.Staged (ReleaseStage (..))
import Hix.Test.Utils (runMTest)

-- | Phases at which the flow can be terminated by failure
data TerminationPhase =
  TerminateAtChecks
  |
  TerminateAtUpload
  |
  NoTerminate
  deriving stock (Eq, Show)

-- | Generated test input for the release property test
data ReleaseInput =
  ReleaseInput {
    commit :: Bool,
    tag :: Bool,
    merge :: Bool,
    check :: Bool,
    candidates :: ArtifactConfig,
    publish :: ArtifactConfig,
    partial :: Bool,
    termination :: TerminationPhase
  }
  deriving stock (Show)

genArtifactConfig :: Gen ArtifactConfig
genArtifactConfig =
  ArtifactConfig <$> Gen.bool <*> Gen.bool

genTermination :: Bool -> Bool -> Gen TerminationPhase
genTermination check hasUploads =
  Gen.element $ [NoTerminate] ++ checksOpt ++ uploadOpt
  where
    checksOpt = [TerminateAtChecks | check]
    uploadOpt = [TerminateAtUpload | hasUploads]

genReleaseInput :: Gen ReleaseInput
genReleaseInput = do
  commit <- Gen.bool
  tag <- Gen.bool
  merge <- Gen.bool
  check <- Gen.bool
  candidates <- genArtifactConfig
  publish <- genArtifactConfig
  partial <- Gen.bool
  let hasUploads = candidates.sources || candidates.docs || publish.sources || publish.docs
  termination <- genTermination check hasUploads
  pure ReleaseInput {commit, tag, merge, check, candidates, publish, partial, termination}

-- | Whether this input should result in the flow being terminated
expectTerminated :: ReleaseInput -> Bool
expectTerminated input = case input.termination of
  TerminateAtChecks -> True
  TerminateAtUpload -> not input.partial
  NoTerminate -> False

contextProto :: ReleaseContextProto
contextProto =
  ReleaseContextProto {
    packages = fromList [
      (LocalPackage "local1", ReleasePackage {
        name = LocalPackage "local1",
        version = [1, 0, 0],
        path = [reldir|packages/local1|]
      })
    ],
    hackage = mempty,
    hooks = [],
    commitExtraArgs = [],
    tagExtraArgs = [],
    managed = True,
    git = Nothing
  }

mkTestConfig :: ReleaseInput -> ReleaseTestConfig
mkTestConfig input =
  ReleaseTest.defaultTestConfig {
    checksPass = input.termination /= TerminateAtChecks,
    uploadErrors = uploadErrs
  }
  where
    uploadErrs
      | TerminateAtUpload <- input.termination = [Error (Fatal "upload rejected") [] Nothing]
      | otherwise = []

mkReleaseConfig :: ReleaseInput -> ReleaseConfig
mkReleaseConfig input =
  def {
    targets = Just (pure "local1"),
    version = Just (VersionIncrement VersionIncrement.Major),
    interactive = False,
    commit = input.commit,
    tag = input.tag,
    merge = input.merge,
    check = input.check,
    candidates = input.candidates,
    publish = input.publish,
    partial = input.partial
  }

mkOptions :: ReleaseInput -> ReleaseOptions
mkOptions input =
  ReleaseOptions {
    context = Left contextProto,
    config = mkReleaseConfig input,
    stateFile = def,
    cabal = def,
    git = Nothing,
    uiDebug = Nothing,
    oldStyleVersion = Nothing,
    oldStylePackages = []
  }

-- | Initialize a git repo in the M monad's root directory with an initial commit.
initGitRepo :: M ()
initGitRepo = do
  root <- appRes.root
  runGitNativeHermetic root "test: init" \ git -> do
    git.cmd_ (["init", "-b", "main"] :: [Text])
    git.cmd_ (["commit", "--allow-empty", "-m", "initial"] :: [Text])

-- | Count commits on a branch.
gitCommitCount :: Text -> M Int
gitCommitCount branch = do
  root <- appRes.root
  runGitNativeHermetic root "test: count" \ git -> do
    let args = ["rev-list", "--count", branch] :: [Text]
    git.cmd args >>= \case
      [n] -> pure (fromMaybe 0 (readMaybe @Int (toString n)))
      _ -> pure 0

-- | Get the current branch name.
gitCurrentBranch :: M Text
gitCurrentBranch = do
  root <- appRes.root
  runGitNativeHermetic root "test: branch" \ git ->
    git.cmd ["rev-parse", "--abbrev-ref", "HEAD"] >>= \case
      (b : _) -> pure b
      _ -> pure ""

-- | List git tags.
gitTags :: M [Text]
gitTags = do
  root <- appRes.root
  runGitNativeHermetic root "test: tags" \ git ->
    git.cmd (["tag", "--list"] :: [Text])

-- | Whether a commit is expected on main (beyond the initial commit).
expectMainCommit :: ReleaseInput -> Bool
expectMainCommit input =
  not (expectTerminated input) && input.commit && input.merge

prop_releaseInput :: Property
prop_releaseInput = withTests 100 $ property do
  input <- forAll genReleaseInput
  let
    testConfig = mkTestConfig input
    options = mkOptions input
  (stage, mainCommits, currentBranch, tags) <- test $ runMTest False do
    initGitRepo
    cabal <- cabalConfig contextProto.hackage options.cabal
    (handlers, _testData) <- ReleaseTest.handlersGitTest testConfig options cabal
    stage <- release handlers options.config contextProto
    mainCommits <- gitCommitCount "main"
    currentBranch <- gitCurrentBranch
    tags <- gitTags
    pure (stage, mainCommits, currentBranch, tags)
  annotateShow input
  let terminated = case stage of
        ReleaseStage {state = ReleaseState {termination = Termination _}} -> True
        _ -> False
  terminated === expectTerminated input
  -- After release, we should be back on main
  currentBranch === "main"
  -- On failure, main should NOT have any new commits (only the initial one)
  when terminated do
    mainCommits === 1
  -- On success with commit+merge, main should have 2 commits
  when (expectMainCommit input) do
    mainCommits === 2
  -- Without commit or merge, main should still have 1 commit
  when (not terminated && not (expectMainCommit input)) do
    mainCommits === 1
  -- Tags should only exist on non-terminated flows with tag=True
  let expectTags = not terminated && input.tag
  null tags === not expectTags

test_releaseInput :: TestTree
test_releaseInput =
  testProperty "release-input" prop_releaseInput
