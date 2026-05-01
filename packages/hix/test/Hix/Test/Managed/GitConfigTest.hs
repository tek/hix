module Hix.Test.Managed.GitConfigTest where

import Data.Bits ((.|.))
import qualified Data.Text as Text
import Exon (exon)
import Hedgehog (TestT, annotate, failure, success, (===))
import Path (Abs, Dir, Path, relfile, toFilePath, (</>))
import System.Posix (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode)
import Test.Tasty (TestTree, testGroup)

import Hix.Data.Monad (M)
import Hix.Managed.Data.GitConfig (GitConfig (..), GitEnvVars (..))
import Hix.Managed.Git (GitNative (..), GitResult (..), gitApiFromConfigM, runGitApi, runGitNativeHermetic)
import Hix.Monad (withTempRoot)
import Hix.Test.Utils (UnitTest, addFile, runMTest, unitTest)

assertGitSuccess ::
  HasCallStack =>
  GitResult ->
  TestT IO [Text]
assertGitSuccess result =
  withFrozenCallStack do
    case result of
      GitSuccess {stdout} -> pure stdout
      GitFailure {stderr} -> do
        annotate [exon|stderr: ##{Text.intercalate ", " stderr}|]
        failure

withProject :: (Path Abs Dir -> M a) -> TestT IO a
withProject test =
  runMTest False do
    withTempRoot "test-git" \ root -> do
      runGitNativeHermetic root "test: init" \ git -> git.cmd_ ["init"]
      test root

withGit ::
  GitConfig ->
  (GitNative -> M a) ->
  TestT IO a
withGit config use =
  withProject \ root ->
    runGitApi (gitApiFromConfigM (Just config) pure) root "test: main" use

withCommit ::
  GitConfig ->
  (GitNative -> M ()) ->
  (GitNative -> M a) ->
  TestT IO (GitResult, a)
withCommit config setup use = do
  withGit config \ git -> do
    setup git
    commitResult <- git.cmdResult ["commit", "--allow-empty", "-m", "success"]
    result <- use git
    pure (commitResult, result)

withCommitSuccess ::
  GitConfig ->
  (GitNative -> M ()) ->
  (GitNative -> M a) ->
  TestT IO a
withCommitSuccess config setup use = do
  (commitResult, result) <- withCommit config setup use
  assertGitSuccess commitResult
  pure result

commitAuthor :: GitConfig -> TestT IO [Text]
commitAuthor config = do
  result <- withCommitSuccess config (const unit) \ git -> git.cmdResult ["show", "--format=%ae"]
  assertGitSuccess result

configHermetic :: GitConfig
configHermetic =
  GitConfig {
    executable = Nothing,
    hooks = Just False,
    env = Just GitEnvHermetic
  }

test_hermeticCommit :: UnitTest
test_hermeticCommit = do
  author <- commitAuthor configHermetic
  author === ["hix-bot@github.com"]

configExplicit :: GitConfig
configExplicit =
  GitConfig {
    executable = Nothing,
    hooks = Just False,
    env = Just $ GitEnvExplicit [
      ("GIT_AUTHOR_EMAIL", "git-test@localhost"),
      ("GIT_COMMITTER_EMAIL", "git-test@localhost"),
      ("GIT_AUTHOR_NAME", "Test"),
      ("GIT_COMMITTER_NAME", "Test"),
      ("GIT_CONFIG_NOSYSTEM", "1")
    ]
  }

-- | Verify that a custom env is applied: commit succeeds with custom author identity.
test_explicitCommit :: UnitTest
test_explicitCommit = do
  author <- commitAuthor configExplicit
  author === ["git-test@localhost"]

-- | Set up a pre-commit hook that always fails in the given directory.
addFailingHook :: GitNative -> M ()
addFailingHook git = do
  addFile git.repo path $ Text.unlines [
    "#!/bin/sh",
    "exit 1"
    ]
  liftIO $ setFileMode (toFilePath (git.repo </> path)) (ownerExecuteMode .|. ownerReadMode .|. ownerWriteMode)
  where
    path = [relfile|.git/hooks/pre-commit|]


-- | Verify that hooks are disabled when noHooks is True (the default).
--   Sets up a pre-commit hook that always fails, then commits; with hooks disabled, the commit succeeds.
test_hooksDisabled :: UnitTest
test_hooksDisabled = do
  withCommitSuccess configHermetic addFailingHook \ _ -> unit

configHooksEnabled :: GitConfig
configHooksEnabled =
  GitConfig {
    executable = Nothing,
    hooks = Just True,
    env = Just GitEnvHermetic
  }

-- | Verify that hooks run when explicitly enabled.
test_hooksEnabled :: UnitTest
test_hooksEnabled = do
  (result, ()) <- withCommit configHooksEnabled addFailingHook \ _ -> unit
  case result of
    GitFailure {} -> success
    GitSuccess {} -> do
      annotate "Expected hook to cause failure, but commit succeeded"
      failure

test_gitConfig :: TestTree
test_gitConfig =
  testGroup "git-config" [
    unitTest "hermetic env" test_hermeticCommit,
    unitTest "explicit env" test_explicitCommit,
    unitTest "hooks disabled" test_hooksDisabled,
    unitTest "hooks enabled" test_hooksEnabled
  ]
