module Hix.Test.Managed.GitFetchTest where

import Hedgehog (TestT, (===))
import Path (Abs, Dir, Path, reldir, toFilePath, (</>))

import Hix.Data.Monad (M)
import Hix.Managed.Data.CreatedRefs (noBranches)
import Hix.Managed.Git (GitNative (..), runGitNativeHermetic)
import Hix.Managed.Git.Native (GitBracketConfig (..), gitBracket)
import Hix.Monad (withTempRoot)
import Hix.Test.Utils (UnitTest, runMTest)

-- | Create a remote with a tagged commit on @main@ and a release branch with another commit on top of the tag.
setupRemote :: Path Abs Dir -> M ()
setupRemote root = do
  runGitNativeHermetic root "test: init remote" \ git -> do
    git.cmd_ ["init", "--bare", "remote.git"]
    git.cmd_ ["init", "-b", "main", "src"]
  runGitNativeHermetic (root </> [reldir|src|]) "test: populate remote" \ git -> do
    git.cmd_ ["commit", "--allow-empty", "-m", "initial"]
    git.cmd_ ["tag", "pkg-1.7.3.0"]
    git.cmd_ ["remote", "add", "origin", remoteUrl root]
    git.cmd_ ["push", "origin", "main", "pkg-1.7.3.0"]
    git.cmd_ ["switch", "--create", "release/pkg/1.7.3.0", "pkg-1.7.3.0"]
    git.cmd_ ["commit", "--allow-empty", "-m", "release"]
    git.cmd_ ["push", "origin", "release/pkg/1.7.3.0"]

remoteUrl :: Path Abs Dir -> Text
remoteUrl root = toText (toFilePath (root </> [reldir|remote.git|]))

-- | Clone like a CI checkout does: shallow and with only the default branch.
cloneCi :: Path Abs Dir -> M ()
cloneCi root =
  runGitNativeHermetic root "test: clone" \ git ->
    git.cmd_ ["clone", "--depth", "1", "--single-branch", "--branch", "main", "file://" <> remoteUrl root, "ci"]

localBranches :: GitNative -> M [Text]
localBranches git =
  git.cmd ["branch", "--list", "--format=%(refname:short)", "release/*"]

-- | Run the bracket in the clone, returning the release branches that are present while it runs.
branchesInBracket :: Bool -> TestT IO [Text]
branchesInBracket fetch =
  runMTest False do
    withTempRoot "test-git-fetch" \ root -> do
      setupRemote root
      cloneCi root
      runGitNativeHermetic (root </> [reldir|ci|]) "test: bracket" \ git ->
        snd <$> gitBracket git GitBracketConfig {push = False, fetch, merge = False} do
          branches <- localBranches git
          pure (branches, noBranches)

test_fetchReleaseBranches :: UnitTest
test_fetchReleaseBranches = do
  withoutFetch <- branchesInBracket False
  [] === withoutFetch
  withFetch <- branchesInBracket True
  ["release/pkg/1.7.3.0"] === withFetch
