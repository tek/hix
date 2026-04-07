module Hix.Integration.ReleaseTest where

import Control.Concurrent (threadDelay)
import Data.IORef (readIORef)
import qualified Data.Text as Text
import Exon (exon)
import Graphics.Vty (Key (..))
import Hedgehog ((===))
import Path (Abs, Dir, Path, reldir, relfile, (</>))
import Test.Tasty (TestTree, testGroup)

import Hix.Data.Options (ReleaseOptions (..))
import Hix.Data.Version (Version)
import Hix.Error (pathText)
import Hix.Integration.Hackage (TestHackage (..), withHackageClient)
import Hix.Integration.Pty (tmuxLiftM, tmuxTest)
import Hix.Integration.Utils (
  UnitTest,
  add,
  addP,
  addP2,
  addP3,
  lib1Hs,
  lib2Hs,
  lib3Hs,
  local1,
  local2,
  local3,
  runMTest,
  unitTest,
  withHixDir,
  )
import Hix.Managed.Cabal.Config (cabalConfig)
import Hix.Managed.Cabal.ContextHackageRepo (unsafeCentralHackageContextFixed)
import Hix.Managed.Cabal.Data.ContextHackageRepo (
  ContextHackageLocation (ContextHackageLocation),
  ContextHackagePassword (PasswordUnobscured),
  ContextHackageRepo (..),
  contextHackageRepo,
  )
import Hix.Managed.Cabal.Upload (revisionCabalFile)
import qualified Hix.Managed.Data.ReleaseConfig as ReleaseConfig
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig, ReleaseVersion (..), bothArtifacts)
import Hix.Managed.Data.ReleaseContext (ReleaseContextProto (..), ReleasePackage (..))
import qualified Hix.Managed.Data.VersionIncrement as VersionIncrement
import Hix.Managed.Flake (runFlakeAt, runFlakeGen, runFlakeLock)
import qualified Hix.Managed.Git as Git
import Hix.Managed.Git (GitNative, runGitNativeHermetic)
import Hix.Managed.Handlers.Context (ContextKey (ContextRelease), jsonOrQueryProd)
import qualified Hix.Managed.Handlers.Release.Test as ReleaseTest
import Hix.Managed.Handlers.Release.Test (ReleaseTestConfig (..))
import Hix.Managed.Release (release, releaseCli)
import Hix.Monad (M, withTempDir, withTempRoot)
import Hix.Test.Hedgehog (eqLines)
import Hix.Ui.Debug (brickDebug, brickDebugAsync, sendChar, sendKey)

-- | Test version of releaseCli that uses unit test handlers for runChecks and uploadArtifact
--   Returns test data for making assertions
releaseCliUnitTest :: ReleaseOptions -> M ReleaseTest.ReleaseTestData
releaseCliUnitTest options = do
  releaseContext <- jsonOrQueryProd ContextRelease options.context
  cabal <- cabalConfig releaseContext.hackage options.cabal
  (handlers, testData) <- ReleaseTest.handlersUnitTest options cabal
  void $ release handlers options.config releaseContext
  pure testData

-- | Test version of releaseCli that uses real git handlers but mock uploads
--   Used for testing git workflows end-to-end
releaseCliGitTest :: ReleaseTest.ReleaseTestConfig -> ReleaseOptions -> M ReleaseTest.ReleaseTestData
releaseCliGitTest testConfig options = do
  releaseContext <- jsonOrQueryProd ContextRelease options.context
  cabal <- cabalConfig releaseContext.hackage options.cabal
  (handlers, testData) <- ReleaseTest.handlersGitTest testConfig options cabal
  void $ release handlers options.config releaseContext
  pure testData

flake :: Bool -> Text -> Text
flake each path =
  [exon|{
  description = "hix test project";
  inputs.hix.url = "#{path}";
  outputs = {hix, ...}: hix.lib.flake {
    compiler = "ghc910";
    managed = {
      enable = true;
      latest.compiler = "ghc910";
      sets = "#{if each then "each" else "all"}";
    };
    compat.enable = false;
    ghcVersions = [];
    cabal = {
      license = "MIT";
      meta = {
        category = "Test";
        maintainer = "hix";
        synopsis = "Synopsis";
      };
    };
    packages = {
      local1 = {
        src = ./packages/local1;
        library = {
          enable = true;
          dependencies = ["base" "semigroups" "extra"];
        };
      };
      local2 = {
        src = ./packages/local2;
        library = {
          enable = true;
          dependencies = ["local1"];
        };
      };
      local3 = {
        src = ./packages/local3;
        library = {
          enable = true;
          dependencies = ["local1" "local2"];
        };
      };
    };
    envs.latest-local1.localPackage = api: api.minimal;
    envs.latest-local2.localPackage = api: api.minimal;
    envs.latest-local3.localPackage = api: api.minimal;
  };
}
|]

initialStateFileEach :: Text
initialStateFileEach =
  [exon|{
  bounds = {
    local1 = {
      base = {
        lower = null;
        upper = "5";
      };
      semigroups = {
        lower = "0.19";
        upper = "0.21";
      };
      extra = {
        lower = "1.7";
        upper = "1.9";
      };
    };
    local2 = {
      base = {
        lower = null;
        upper = "5";
      };
      local1 = {
        lower = "0";
        upper = "1.2";
      };
    };
    local3 = {
      base = {
        lower = null;
        upper = "5";
      };
      local1 = {
        lower = "0";
        upper = "1.2";
      };
      local2 = {
        lower = "0";
        upper = "2.3";
      };
    };
  };
  versions = {
    latest-local1 = {
      base = "4.19.0.0";
      semigroups = "0.19.2";
      extra = "1.7.16";
    };
    latest-local2 = {
      local1 = "1.1.1";
    };
    latest-local3 = {
      local1 = "1.1.1";
      local2 = "2.2.2";
    };
  };
  overrides = {
    latest-local1 = {};
    latest-local2 = {};
    latest-local2 = {};
  };
  initial = {
    latest-local1 = {
      base = "4.19.0.0";
      semigroups = "0.19.2";
      extra = "1.7.16";
    };
    latest-local2 = {
      local1 = "1.1.1";
    };
    latest-local3 = {
      local1 = "1.1.1";
      local2 = "2.2.2";
    };
  };
  solver = {};
  packages = {};
  resolving = false;
}
|]

initialStateFileAll :: Text
initialStateFileAll =
  [exon|{
  bounds = {
    local1 = {
      base = {
        lower = null;
        upper = "5";
      };
      semigroups = {
        lower = "0.19";
        upper = "0.21";
      };
      extra = {
        lower = "1.7";
        upper = "1.9";
      };
    };
    local2 = {
      base = {
        lower = null;
        upper = "5";
      };
      local1 = {
        lower = "0";
        upper = "1.2";
      };
    };
    local3 = {
      base = {
        lower = null;
        upper = "5";
      };
      local1 = {
        lower = "0";
        upper = "1.2";
      };
      local2 = {
        lower = "0";
        upper = "2.3";
      };
    };
  };
  versions = {
    latest = {
      base = "4.19.0.0";
      semigroups = "0.19.2";
      extra = "1.7.16";
    };
  };
  overrides = {
    latest = {};
  };
  initial = {
    latest = {
      base = "4.19.0.0";
      semigroups = "0.19.2";
      extra = "1.7.16";
    };
  };
  solver = {};
  packages = {};
  resolving = false;
}
|]

setupProject :: Text -> Bool -> GitNative -> M (Path Abs Dir)
setupProject hixRoot each git = do
  git.cmd_ ["init"]
  add git [relfile|flake.nix|] (flake each hixRoot)
  add git [relfile|ops/managed.nix|] (if each then initialStateFileEach else initialStateFileAll)
  addP git [relfile|lib/Lib1.hs|] lib1Hs
  addP2 git [relfile|lib/Lib2.hs|] lib2Hs
  addP3 git [relfile|lib/Lib3.hs|] lib3Hs
  runFlakeLock (runFlakeAt git.repo)
  git.cmd_ ["add", "flake.lock"]
  runFlakeGen (runFlakeAt git.repo)
  git.cmd_ ["add", pathText (local1 </> [relfile|local1.cabal|])]
  git.cmd_ ["add", pathText (local2 </> [relfile|local2.cabal|])]
  git.cmd_ ["add", pathText (local3 </> [relfile|local3.cabal|])]
  git.cmd_ ["commit", "-m", "1"]
  pure (git.repo </> local1)

targetCabal :: Text
targetCabal =
  [exon|cabal-version: 1.12

-- This file has been generated from package.yaml by hpack version 0.38.2.
--
-- see: https://github.com/sol/hpack

name:           local1
version:        1.2.0
synopsis:       Synopsis
description:    See https://hackage.haskell.org/package/local1/docs/Local1.html
category:       Test
maintainer:     hix
license:        MIT
build-type:     Simple

library
  exposed-modules:
      Lib1
  other-modules:
      Paths_local1
  hs-source-dirs:
      lib
  build-depends:
      base <5
    , extra >=1.7 && <1.9
    , semigroups >=0.19 && <0.21
  default-language: Haskell2010
|]

context :: TestHackage -> ReleaseContextProto
context hackage =
  ReleaseContextProto {
    packages = [
      ("local1", ReleasePackage {
        name = "local1",
        version = [1, 1, 1],
        path = [reldir|local1|]
      }),
      ("local2", ReleasePackage {
        name = "local2",
        version = [2, 2, 2],
        path = [reldir|local2|]
      }),
      ("local3", ReleasePackage {
        name = "local3",
        version = [3, 3, 3],
        path = [reldir|local3|]
      })
    ],
    hackage = [unsafeCentralHackageContextFixed, ("test", hackage.context)],
    hooks = [],
    commitExtraArgs = [],
    tagExtraArgs = [],
    managed = True
  }

-- | Mock Hackage context for unit tests that allows publishing without a server
mockPublishHackageContext :: ContextHackageRepo
mockPublishHackageContext =
  (contextHackageRepo "test-mock") {
    description = Just "mock for unit tests",
    location = Just (ContextHackageLocation "http://localhost:8080"),
    user = Just "test",
    password = Just (PasswordUnobscured "test"),
    solver = Just True,
    publish = Just True
  }

-------------------------------------------------------------------------------
-- Test contexts
-------------------------------------------------------------------------------

-- | Context for tests that don't need a real Hackage server
--   Uses a mock Hackage repo that allows all operations
contextMock :: ReleaseContextProto
contextMock = contextMockVersions [[1, 1, 1], [2, 2, 2]]

-- | Context with updated versions (after Phase 1 increments)
contextMockPhase2 :: ReleaseContextProto
contextMockPhase2 = contextMockVersions [[1, 1, 2], [2, 2, 3]]

contextMockVersions :: [Version] -> ReleaseContextProto
contextMockVersions [v1, v2] =
  ReleaseContextProto {
    packages = [
      ("local1", ReleasePackage {
        name = "local1",
        version = v1,
        path = [reldir|local1|]
      }),
      ("local2", ReleasePackage {
        name = "local2",
        version = v2,
        path = [reldir|local2|]
      })
    ],
    hackage = [unsafeCentralHackageContextFixed, ("test", mockPublishHackageContext)],
    hooks = [],
    commitExtraArgs = [],
    tagExtraArgs = [],
    managed = True
  }
contextMockVersions _ = error "contextMockVersions: expected two versions"

-------------------------------------------------------------------------------
-- Test configurations
-------------------------------------------------------------------------------

-- | Config for interactive tests (version selection UI)
configInteractive :: ReleaseConfig
configInteractive =
  def {
    ReleaseConfig.publish = bothArtifacts,
    ReleaseConfig.targets = Just ["local1", "local2"],
    ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
    ReleaseConfig.interactive = True
  }

-- | Config for Phase 1: candidates, run checks, commit and push (no tags yet)
configPhase1 :: ReleaseConfig
configPhase1 =
  def {
    ReleaseConfig.candidates = bothArtifacts,
    ReleaseConfig.publish = ReleaseConfig.noArtifacts,
    ReleaseConfig.targets = Just ["local1", "local2"],
    ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Minor),
    ReleaseConfig.interactive = False,
    ReleaseConfig.check = True,
    ReleaseConfig.commit = True,
    ReleaseConfig.tag = False,
    ReleaseConfig.push = True
  }

-- | Config for Phase 2: publish, run checks, create and push tags (no commit)
configPhase2 :: ReleaseConfig
configPhase2 =
  def {
    ReleaseConfig.candidates = ReleaseConfig.noArtifacts,
    ReleaseConfig.publish = bothArtifacts,
    ReleaseConfig.targets = Just ["local1", "local2"],
    ReleaseConfig.version = Just KeepVersion,
    ReleaseConfig.interactive = False,
    ReleaseConfig.check = True,
    ReleaseConfig.commit = False,
    ReleaseConfig.tag = True,
    ReleaseConfig.push = True
  }


steps :: [(Char, Text)]
steps =
  [
    ('j', ""),
    ('l', ""),
    ('b', ""),
    ('j', ""),
    ('t', "")
  ]

releaseTest :: Bool -> UnitTest
releaseTest each = do
  debug <- brickDebug
  withHixDir \ hixRoot -> do
    cabalContents <- tmuxTest False 20 do
      tmuxLiftM do
        withTempRoot "test-release" \ root ->
          withHackageClient \ hackage -> do
            _packageDir <- runGitNativeHermetic root "test: project setup" (setupProject hixRoot each)
            let
              options = ReleaseOptions {
                context = Left (context hackage),
config = configInteractive,
                stateFile = def,
                cabal = def,
                uiDebug = Just debug,
                oldStyleVersion = Nothing,
                oldStylePackages = []
              }
            brickDebugAsync debug
              do
                releaseCli options
                revisionCabalFile hackage.client "local1-1.2.0" 0
              \ waitForApp -> do
                waitForApp
                for_ steps \ (c, _target) -> do
                  sendChar debug c
                sendKey debug KEnter
                waitForApp
                -- Checks targets selection
                sendKey debug KEnter
                waitForApp
                -- Upload stage: candidate sources
                sendKey debug KEnter
                waitForApp
                -- Upload stage: candidate docs
                sendKey debug KEnter
                waitForApp
                -- Upload stage: publish sources
                sendKey debug KEnter
                waitForApp
                -- Upload stage: publish docs
                sendKey debug KEnter
    eqLines targetCabal cabalContents

test_release_each :: TestTree
test_release_each =
  unitTest "per-package sets" (releaseTest True)

test_release_all :: TestTree
test_release_all =
  unitTest "shared set" (releaseTest False)

-- | Test data returned from two-phase release workflow
data GitWorkflowResult =
  GitWorkflowResult {
    checksRan1 :: Bool,
    uploaded1 :: Int,
    tagsBefore :: [Text],
    checksRan2 :: Bool,
    uploaded2 :: Int,
    tagsAfter :: [Text],
    remoteTags :: [Text],
    remoteBranchesAfterPhase1 :: [Text],
    remoteBranchesAfterPhase2 :: [Text]
  }
  deriving stock (Eq, Show)

-- | Test two-phase release workflow with git integration
--   Simulates a Github workflow where:
--   1. Phase 1: Run checks and upload candidates (triggered by manual dispatch)
--   2. (Simulated PR review/merge)
--   3. Phase 2: Upload permanent artifacts with git commit/tag/push (triggered by merge)
test_releaseGitWorkflow :: UnitTest
test_releaseGitWorkflow = do
  withHixDir \ hixRoot -> do
    result <- runMTest False do
      -- Create bare repo to act as "remote" (like GitHub)
      withTempDir "remote" \ remoteDir -> do
        runGitNativeHermetic remoteDir "test: init bare remote" \ remoteGit -> do
          remoteGit.cmd_ ["init", "--bare"]

        withTempRoot "test-release-git" \root -> do
          (phase1Tags, phase2Tags, phase1Data, phase2Data, remoteBranchesAfterPhase1, remoteBranchesAfterPhase2) <-
            runGitNativeHermetic root "test: project setup and release" \ git -> do
              -- Set up project
              void $ setupProject hixRoot False git

              -- Add the remote
              git.cmd_ ["remote", "add", "origin", pathText remoteDir]

              -- Push initial commit
              git.cmd_ ["push", "-u", "origin", "master"]

              -- Phase 1: Candidates upload with git operations
              let
                testConfig = ReleaseTestConfig {checksPass = True, uploadErrors = [], failPackages = []}
                options1 = ReleaseOptions {
                  context = Left contextMock,
                  config = configPhase1,
                  stateFile = def,
                  cabal = def,
                  uiDebug = Nothing,
                  oldStyleVersion = Nothing,
                  oldStylePackages = []
                }
              testData1 <- releaseCliGitTest testConfig options1

              -- Capture remote branches after phase 1
              remoteBranchesAfterPhase1 <- runGitNativeHermetic remoteDir "test: check remote after phase 1" \ remoteGit ->
                remoteGit.cmd ["branch", "--format=%(refname:short)"]

              -- Verify tags weren't created in phase 1
              tagsBefore <- git.cmd ["tag", "-l"]

              -- === Simulate PR merge: merge release branch into master ===
              -- In a real workflow, a PR would be created and merged via GitHub.
              -- Find the release branch (format: release-prepare-<epoch>) and merge it.
              branches <- git.cmd ["branch", "--format=%(refname:short)"]
              releaseBranch <- case filter ("release-prepare-" `Text.isPrefixOf`) branches of
                [b] -> pure b
                found -> error [exon|Expected one release branch, found: #{show found}|]
              git.cmd_ ["checkout", "master"]
              git.cmd_ ["merge", releaseBranch, "--no-ff", "-m", "Merge release branch"]

              -- Ensure a different epoch for Phase 2's branch name (release-<epoch> format)
              liftIO $ threadDelay 1_100_000

              -- Phase 2: Publish upload, create and push tags
              let
                options2 = ReleaseOptions {
                  context = Left contextMockPhase2,
                  config = configPhase2,
                  stateFile = def,
                  cabal = def,
                  uiDebug = Nothing,
                  oldStyleVersion = Nothing,
                  oldStylePackages = []
                }
              testData2 <- releaseCliGitTest testConfig options2

              -- Capture remote branches after phase 2
              remoteBranchesAfterPhase2 <- runGitNativeHermetic remoteDir "test: check remote after phase 2" \ remoteGit ->
                remoteGit.cmd ["branch", "--format=%(refname:short)"]

              -- Verify tags were created
              tagsAfter <- git.cmd ["tag", "-l"]

              pure (tagsBefore, tagsAfter, testData1, testData2, remoteBranchesAfterPhase1, remoteBranchesAfterPhase2)

          -- Verify remote state
          remoteTags <- runGitNativeHermetic remoteDir "test: verify remote state" \ remoteGit -> do
            remoteGit.cmd ["tag", "-l"]

          -- Collect assertions data
          checksRan1 <- liftIO $ readIORef phase1Data.checksRun
          uploaded1 <- liftIO $ length <$> readIORef phase1Data.uploadedArtifacts
          checksRan2 <- liftIO $ readIORef phase2Data.checksRun
          uploaded2 <- liftIO $ length <$> readIORef phase2Data.uploadedArtifacts

          pure GitWorkflowResult {
            checksRan1,
            uploaded1,
            tagsBefore = phase1Tags,
            checksRan2,
            uploaded2,
            tagsAfter = phase2Tags,
            remoteTags,
            remoteBranchesAfterPhase1,
            remoteBranchesAfterPhase2
          }

-- Assertions in TestT IO context (inside withHixDir)
    result.checksRan1 === True
    result.uploaded1 === 4  -- 2 packages × 2 candidate stages
    result.tagsBefore === []  -- No tags after phase 1 (tags created after publish)
    result.checksRan2 === True  -- checks enabled in phase 2
    result.uploaded2 === 4  -- 2 packages × 2 publish stages
    sort result.tagsAfter === ["local1-1.1.2", "local2-2.2.3"]  -- Tags created in phase 2
    sort result.remoteTags === ["local1-1.1.2", "local2-2.2.3"]  -- Tags pushed in phase 2
    -- Phase 1 pushes the release branch (+ master)
    length result.remoteBranchesAfterPhase1 === 2
    -- Phase 2 should NOT push any new branches (only tags)
    result.remoteBranchesAfterPhase2 === result.remoteBranchesAfterPhase1

test_release :: TestTree
test_release =
  testGroup "release" [
    test_release_each,
    test_release_all,
    unitTest "git-workflow" test_releaseGitWorkflow
  ]


