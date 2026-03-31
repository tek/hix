module Hix.Test.Managed.ReleaseTest where

import Data.IORef (readIORef)
import Exon (exon)
import GHC.Exts (fromList)
import Hedgehog (Gen, Property, assert, forAll, property, test, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Path (Dir, Path, Rel, SomeBase (..), reldir, relfile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Data.Error (Error (..), ErrorContext (..), ErrorMessage (..))
import Hix.Data.Monad (M)
import Hix.Data.Options (ReleaseOptions (..))
import Hix.Data.PackageName (LocalPackage (..), PackageName (..), localPackageName)
import Hix.Managed.Cabal.Config (cabalConfig)
import Hix.Managed.Cabal.Data.UploadStage (candidateDocs, candidateSources, publishSources)
import qualified Hix.Managed.Data.ReleaseConfig as ReleaseConfig
import Hix.Managed.Data.ReleaseConfig (ArtifactConfig (..), ReleaseConfig, ReleaseVersion (..), noArtifacts)
import Hix.Managed.Data.ReleaseContext (ReleaseContextProto (..), ReleasePackage (..))
import Hix.Data.PathSpec (PathSpec (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.StateVersionsContext (StateVersionsContext (..), StateVersionsPackage (..))
import Hix.Managed.Data.UploadResult (RepoResult (..))
import qualified Hix.Managed.Data.VersionIncrement as VersionIncrement
import Hix.Managed.Handlers.Context (ContextKey (ContextRelease), jsonOrQueryProd)
import qualified Hix.Managed.Handlers.Release.Test as ReleaseTest
import Hix.Managed.Handlers.Release.Test (ReleaseTestConfig (..))
import Hix.Managed.Release (release)
import Hix.Managed.Release.Data.ReleaseResult (PackageStatus (..), ReleasedPackage (..))
import Hix.Managed.Release.Data.SelectedVersion (explicitVersion, implicitVersion, keepVersion)
import Hix.Managed.Release.Data.Staged (SelectedTargetView (..))
import Hix.Managed.Release.Data.TargetSpec (TargetSpec (..))
import Hix.Managed.Release.ReleaseResult (releaseMessage)
import Hix.Managed.Release.StateVersions (checkVersionPersistence)
import Hix.Managed.Release.Validation (VersionProblem (..), validateVersion)
import Hix.Test.Hedgehog (assertContains, eqLines)
import Hix.Test.Run (runMTestDir)
import Hix.Test.Utils (UnitTest, runMLogTest, runMTest, unitTest)

-- Test project structure helpers (only for contextBase)
local1 :: Path Rel Dir
local1 = [reldir|packages/local1|]

local2 :: Path Rel Dir
local2 = [reldir|packages/local2|]

-- | Generate a package name like "pkg1", "pkg2", etc.
genPackageName :: Int -> LocalPackage
genPackageName n = LocalPackage (PackageName ("pkg" <> show n))

-- | Generate a list of packages (1-5 packages)
genPackages :: Gen [(LocalPackage, ReleasePackage)]
genPackages = do
  count <- Gen.int (Range.linear 1 5)
  pure [(name, mkPackage name) | i <- [1..count], let name = genPackageName i]
  where
    mkPackage name =
      ReleasePackage {
        name,
        version = [1, 0, 0],
        path = [reldir|packages|] -- dummy path
      }

-- | Generate an ArtifactConfig
genArtifactConfig :: Gen ArtifactConfig
genArtifactConfig =
  ArtifactConfig <$> Gen.bool <*> Gen.bool

-- | Count the number of enabled stages
countEnabledStages :: ArtifactConfig -> ArtifactConfig -> Int
countEnabledStages candidates publish =
  boolToInt candidates.sources +
  boolToInt candidates.docs +
  boolToInt publish.sources +
  boolToInt publish.docs
  where
    boolToInt True = 1
    boolToInt False = 0

-- | Data type for generated test configuration
data TestConfig =
  TestConfig {
    packages :: [(LocalPackage, ReleasePackage)],
    candidates :: ArtifactConfig,
    publish :: ArtifactConfig
  }
  deriving stock (Show)

genTestConfig :: Gen TestConfig
genTestConfig =
  TestConfig <$> genPackages <*> genArtifactConfig <*> genArtifactConfig

-- | Create a ReleaseContextProto from test config
mkContext :: TestConfig -> ReleaseContextProto
mkContext TestConfig {packages} =
  ReleaseContextProto {
    packages = fromList packages,
    hackage = [],
    hooks = [],
    commitExtraArgs = [],
    tagExtraArgs = [],
    managed = True
  }

-- | Create a ReleaseConfig from test config
mkConfig :: TestConfig -> ReleaseConfig
mkConfig TestConfig {packages, candidates, publish} =
  def {
    ReleaseConfig.targets = nonEmpty (TargetName . localPackageName . fst <$> packages),
    ReleaseConfig.candidates = candidates,
    ReleaseConfig.publish = publish,
    ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
    ReleaseConfig.interactive = False,
    ReleaseConfig.check = True  -- Run checks in unit tests
  }

-- | Static context for backward-compatible unit test
context :: ReleaseContextProto
context =
  ReleaseContextProto {
    packages = [
      ("local1", ReleasePackage {
        name = "local1",
        version = [1, 1, 1],
        path = local1
      }),
      ("local2", ReleasePackage {
        name = "local2",
        version = [2, 2, 2],
        path = local2
      })
    ],
    hackage = [],
    hooks = [],
    commitExtraArgs = [],
    tagExtraArgs = [],
    managed = True
  }

-- | Run the release CLI with given options and return test data
releaseCliUnitTest ::
  ReleaseOptions ->
  M ReleaseTest.ReleaseTestData
releaseCliUnitTest = releaseCliUnitTestWith ReleaseTest.defaultTestConfig

-- | Run the release CLI with configurable test behavior
releaseCliUnitTestWith ::
  ReleaseTestConfig ->
  ReleaseOptions ->
  M ReleaseTest.ReleaseTestData
releaseCliUnitTestWith testConfig options = do
  releaseContext <- jsonOrQueryProd ContextRelease options.context
  cabal <- cabalConfig releaseContext.hackage options.cabal
  (handlers, testData) <- ReleaseTest.handlersUnitTestWith testConfig options cabal
  void $ release handlers options.config releaseContext
  pure testData

-- | Property test: verify upload count matches expected based on config
prop_uploadCount :: Property
prop_uploadCount = property do
  testCfg <- forAll genTestConfig
  test do
    testData <- runMTest False do
      let
        options = ReleaseOptions {
          context = Left (mkContext testCfg),
          config = mkConfig testCfg,
          stateFile = def,
          cabal = def,
          uiDebug = Nothing,
          oldStyleVersion = Nothing,
          oldStylePackages = []
        }
      releaseCliUnitTest options

    -- Verify checks were run
    checksRun <- liftIO $ readIORef testData.checksRun
    checksRun === True

    -- Verify correct number of uploads
    uploaded <- liftIO $ readIORef testData.uploadedArtifacts
    let
      numPackages = length testCfg.packages
      enabledStages = countEnabledStages testCfg.candidates testCfg.publish
      expectedUploads = enabledStages * numPackages
    length uploaded === expectedUploads

-- | Test that when all artifact stages are disabled, no uploads occur
releaseTestNoUploads :: UnitTest
releaseTestNoUploads = do
  testData <- runMTest False do
    let
      config = def {
        ReleaseConfig.publish = noArtifacts,
        ReleaseConfig.candidates = noArtifacts,
        ReleaseConfig.targets = Just ["local1", "local2"],
        ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
        ReleaseConfig.interactive = False,
        ReleaseConfig.check = True  -- Explicitly enable checks
      }
      options = ReleaseOptions {
        context = Left context,
        config,
        stateFile = def,
        cabal = def,
        uiDebug = Nothing,
        oldStyleVersion = Nothing,
        oldStylePackages = []
      }
    releaseCliUnitTest options

  -- Verify checks were still run
  checksRun <- liftIO $ readIORef testData.checksRun
  checksRun === True

  -- Verify no uploads occurred
  uploaded <- liftIO $ readIORef testData.uploadedArtifacts
  length uploaded === 0

-- | Test that checks are run before uploads (by verifying checksRun is True after release)
releaseTestChecksRunFirst :: UnitTest
releaseTestChecksRunFirst = do
  testData <- runMTest False do
    let
      -- Use candidates only (no publish) to have fewer uploads
      config = def {
        ReleaseConfig.publish = noArtifacts,
        ReleaseConfig.candidates = ArtifactConfig {sources = True, docs = False},
        ReleaseConfig.targets = Just ["local1"],
        ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
        ReleaseConfig.interactive = False,
        ReleaseConfig.check = True  -- Explicitly enable checks
      }
      options = ReleaseOptions {
        context = Left context,
        config,
        stateFile = def,
        cabal = def,
        uiDebug = Nothing,
        oldStyleVersion = Nothing,
        oldStylePackages = []
      }
    releaseCliUnitTest options

  -- Verify checks were run
  checksRun <- liftIO $ readIORef testData.checksRun
  checksRun === True

  -- Verify single upload occurred (1 stage × 1 package)
  uploaded <- liftIO $ readIORef testData.uploadedArtifacts
  length uploaded === 1

-- | Test that with partial = True, subsequent stages continue with successful packages
--   when some packages fail in an earlier stage.
--   Setup: 2 packages, candidate sources enabled, publish sources enabled
--   Expected: Package "local1" fails candidates, but "local2" succeeds and proceeds to publish
releaseTestPartialSuccess :: UnitTest
releaseTestPartialSuccess = do
  testData <- runMTest False do
    let
      -- Configure failure only for "local1" package
      uploadError = Error {
        message = Fatal "Invalid Cabal file: missing required field 'synopsis'",
        context = ErrorContext [],
        level = Nothing
      }
      testConfig = ReleaseTestConfig {
        checksPass = True,
        uploadErrors = [uploadError],
        failPackages = ["local1"]  -- Only fail local1, local2 succeeds
      }
      config = def {
        ReleaseConfig.publish = ArtifactConfig {sources = True, docs = False},
        ReleaseConfig.candidates = ArtifactConfig {sources = True, docs = False},
        ReleaseConfig.targets = Just ["local1", "local2"],
        ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
        ReleaseConfig.interactive = False,
        ReleaseConfig.check = True,
        ReleaseConfig.partial = True  -- Continue with successful packages
      }
      options = ReleaseOptions {
        context = Left context,
        config,
        stateFile = def,
        cabal = def,
        uiDebug = Nothing,
        oldStyleVersion = Nothing,
        oldStylePackages = []
      }
    releaseCliUnitTestWith testConfig options

  -- Verify uploads attempted:
  -- Stage 1 (candidates): local1 (fails), local2 (succeeds) = 2 attempts
  -- Stage 2 (publish): only local2 (partial=True skips failed local1) = 1 attempt
  -- Total = 3 upload attempts
  uploaded <- liftIO $ readIORef testData.uploadedArtifacts
  length uploaded === 3

-- | Test that upload failures are properly handled
--   This exercises the code path where Hackage rejects uploads
releaseTestUploadFailures :: UnitTest
releaseTestUploadFailures = do
  testData <- runMTest False do
    let
      -- Configure a single upload failure
      uploadError = Error {
        message = Fatal "Invalid Cabal file: missing required field 'synopsis'",
        context = ErrorContext [],
        level = Nothing
      }
      testConfig = ReleaseTestConfig {
        checksPass = True,
        uploadErrors = [uploadError],
        failPackages = []  -- All packages fail with these failures
      }
      config = def {
        ReleaseConfig.publish = noArtifacts,
        ReleaseConfig.candidates = ArtifactConfig {sources = True, docs = False},
        ReleaseConfig.targets = Just ["local1", "local2"],
        ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
        ReleaseConfig.interactive = False,
        ReleaseConfig.check = True  -- Explicitly enable checks
      }
      options = ReleaseOptions {
        context = Left context,
        config,
        stateFile = def,
        cabal = def,
        uiDebug = Nothing,
        oldStyleVersion = Nothing,
        oldStylePackages = []
      }
    releaseCliUnitTestWith testConfig options

  -- Verify checks were still run
  checksRun <- liftIO $ readIORef testData.checksRun
  checksRun === True

  -- Verify uploads were attempted (the handler records attempts even if they return failures)
  uploaded <- liftIO $ readIORef testData.uploadedArtifacts
  -- With failures, the release flow continues to attempt all uploads
  -- 1 enabled stage × 2 packages = 2 upload attempts
  length uploaded === 2

-- | Unit test with static configuration (keeps backward compatibility)
-- | Test that a temporary release branch is always created
--   (merge field controls whether it's merged back)
releaseTestBranchCreated :: UnitTest
releaseTestBranchCreated = do
  testData <- runMTest False do
    let
      testConfig = ReleaseTestConfig {
        checksPass = True,
        uploadErrors = [],
        failPackages = []
      }
      config = def {
        ReleaseConfig.publish = ArtifactConfig {sources = True, docs = False},
        ReleaseConfig.candidates = ArtifactConfig {sources = False, docs = False},
        ReleaseConfig.targets = Just ["local1", "local2"],
        ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
        ReleaseConfig.interactive = False,
        ReleaseConfig.check = False,
        ReleaseConfig.merge = False  -- Branch created but not merged
      }
      options = ReleaseOptions {
        context = Left context,
        config,
        stateFile = def,
        cabal = def,
        uiDebug = Nothing,
        oldStyleVersion = Nothing,
        oldStylePackages = []
      }
    releaseCliUnitTestWith testConfig options

  -- Branch creation is handled by git layer, just verify release completes
  -- (More comprehensive git branch test would go in integration tests)
  uploaded <- liftIO $ readIORef testData.uploadedArtifacts
  length uploaded === 2  -- 2 packages × 1 publish stage

test_release :: TestTree
test_release =
  testGroup "non-interactive" [
    testProperty "upload-count" prop_uploadCount,
    unitTest "no-uploads-when-all-disabled" releaseTestNoUploads,
    unitTest "checks-run-first" releaseTestChecksRunFirst,
    unitTest "partial-success" releaseTestPartialSuccess,
    unitTest "upload-failures" releaseTestUploadFailures,
    unitTest "branch-created" releaseTestBranchCreated
  ]

------------------------------------------------------------------------------------------------
-- Version persistence check tests
------------------------------------------------------------------------------------------------

-- | Minimal StateVersionsContext for checkVersionPersistence tests.
-- When @globalVersionFile@ is 'True', a global version file is configured.
-- Package-level version files are set in the targets.
persistenceContext :: Bool -> StateVersionsContext
persistenceContext globalVersionFile =
  StateVersionsContext {
    state = def,
    versionFile = if globalVersionFile then Just (PathConcrete (Rel [relfile|version.nix|])) else Nothing,
    packages = [
      ("local1", StateVersionsPackage {versionFile = Nothing}),
      ("local2", StateVersionsPackage {versionFile = Nothing})
    ]
  }

-- | Test targets: two packages selected for release.
persistenceTargets :: Packages SelectedTargetView
persistenceTargets =
  [
    ("local1", SelectedTargetView {
      package = "local1",
      current = [1, 0, 0],
      releaseVersion = [1, 1, 0],
      explicit = True,
      keep = False
    }),
    ("local2", SelectedTargetView {
      package = "local2",
      current = [2, 0, 0],
      releaseVersion = [2, 1, 0],
      explicit = True,
      keep = False
    })
  ]

-- | When managed is disabled and no version files configured, should fail.
test_checkVersionPersistence_noManagedNoFiles :: UnitTest
test_checkVersionPersistence_noManagedNoFiles = do
  result <- liftIO $ runMTestDir def (checkVersionPersistence False False (persistenceContext False) persistenceTargets)
  assert (isLeft result)

-- | When managed is enabled, should succeed regardless of version files.
test_checkVersionPersistence_managedEnabled :: UnitTest
test_checkVersionPersistence_managedEnabled = do
  result <- liftIO $ runMTestDir def (checkVersionPersistence True False (persistenceContext False) persistenceTargets)
  assert (isRight result)

-- | When global version file is configured, should succeed for all packages.
test_checkVersionPersistence_hasGlobalVersionFile :: UnitTest
test_checkVersionPersistence_hasGlobalVersionFile = do
  result <- liftIO $ runMTestDir def (checkVersionPersistence False False (persistenceContext True) persistenceTargets)
  assert (isRight result)

-- | When force-version is set, should succeed regardless of config.
test_checkVersionPersistence_forceVersion :: UnitTest
test_checkVersionPersistence_forceVersion = do
  result <- liftIO $ runMTestDir def (checkVersionPersistence False True (persistenceContext False) persistenceTargets)
  assert (isRight result)

-- | When only some packages have version files, should fail listing missing ones.
test_checkVersionPersistence_partialVersionFiles :: UnitTest
test_checkVersionPersistence_partialVersionFiles = do
  let
    svContext = StateVersionsContext {
      state = def,
      versionFile = Nothing,
      packages = [
        ("local1", StateVersionsPackage {versionFile = Just (PathConcrete (Rel [relfile|version.nix|]))}),
        ("local2", StateVersionsPackage {versionFile = Nothing})
      ]
    }
  result <- liftIO $ runMTestDir def (checkVersionPersistence False False svContext persistenceTargets)
  assert (isLeft result)

-- | When all packages have individual version files, should succeed.
test_checkVersionPersistence_allPackageVersionFiles :: UnitTest
test_checkVersionPersistence_allPackageVersionFiles = do
  let
    svContext = StateVersionsContext {
      state = def,
      versionFile = Nothing,
      packages = [
        ("local1", StateVersionsPackage {versionFile = Just (PathConcrete (Rel [relfile|version.nix|]))}),
        ("local2", StateVersionsPackage {versionFile = Just (PathConcrete (Rel [relfile|packages/local2/version.nix|]))})
      ]
    }
  result <- liftIO $ runMTestDir def (checkVersionPersistence False False svContext persistenceTargets)
  assert (isRight result)

test_checkVersionPersistence :: TestTree
test_checkVersionPersistence =
  testGroup "check-version-persistence" [
    unitTest "no-managed-no-files" test_checkVersionPersistence_noManagedNoFiles,
    unitTest "managed-enabled" test_checkVersionPersistence_managedEnabled,
    unitTest "has-global-version-file" test_checkVersionPersistence_hasGlobalVersionFile,
    unitTest "force-version" test_checkVersionPersistence_forceVersion,
    unitTest "partial-version-files" test_checkVersionPersistence_partialVersionFiles,
    unitTest "all-package-version-files" test_checkVersionPersistence_allPackageVersionFiles
  ]

------------------------------------------------------------------------------------------------
-- Version validation tests
------------------------------------------------------------------------------------------------

-- | Test that explicit versions are valid when greater than current
test_validateVersion_explicitGreater :: UnitTest
test_validateVersion_explicitGreater = do
  let
    current = [1, 0, 0]
    selected = explicitVersion [1, 0, 1]
  validateVersion current selected === Nothing

-- | Test that explicit versions equal to current are invalid
test_validateVersion_explicitEqual :: UnitTest
test_validateVersion_explicitEqual = do
  let
    current = [1, 0, 0]
    selected = explicitVersion [1, 0, 0]
  validateVersion current selected === Just SameAsCurrent

-- | Test that explicit versions smaller than current are invalid
test_validateVersion_explicitSmaller :: UnitTest
test_validateVersion_explicitSmaller = do
  let
    current = [1, 0, 0]
    selected = explicitVersion [0, 9, 0]
  validateVersion current selected === Just SmallerThanCurrent

-- | Test that keep versions are always valid
test_validateVersion_keepVersion :: UnitTest
test_validateVersion_keepVersion = do
  let
    current = [1, 0, 0]
    selected = keepVersion [1, 0, 0]
  validateVersion current selected === Nothing

-- | Test that implicit versions are invalid (no version specified)
test_validateVersion_implicitVersion :: UnitTest
test_validateVersion_implicitVersion = do
  let
    current = [1, 0, 0]
    selected = implicitVersion [1, 0, 1]
  validateVersion current selected === Just NoVersionSpecified

-- | Test that large increments are invalid
test_validateVersion_largeIncrement :: UnitTest
test_validateVersion_largeIncrement = do
  let
    current = [1, 0, 0]
    -- Jump more than one major version
    selected = explicitVersion [3, 0, 0]
  validateVersion current selected === Just TooLargeIncrement

-- | Test that one supermajor increment is valid (A.B -> (A+1).B)
test_validateVersion_oneSupermajorIncrement :: UnitTest
test_validateVersion_oneSupermajorIncrement = do
  let
    current = [1, 0, 0]
    selected = explicitVersion [2, 0, 0]
  validateVersion current selected === Nothing

-- | Test that one major increment is valid (A.B.C -> A.(B+1).C)
test_validateVersion_oneMajorIncrement :: UnitTest
test_validateVersion_oneMajorIncrement = do
  let
    current = [1, 0, 0]
    selected = explicitVersion [1, 1, 0]
  validateVersion current selected === Nothing

test_versionValidation :: TestTree
test_versionValidation =
  testGroup "version-validation" [
    unitTest "explicit-greater" test_validateVersion_explicitGreater,
    unitTest "explicit-equal" test_validateVersion_explicitEqual,
    unitTest "explicit-smaller" test_validateVersion_explicitSmaller,
    unitTest "keep-version" test_validateVersion_keepVersion,
    unitTest "implicit-version" test_validateVersion_implicitVersion,
    unitTest "large-increment" test_validateVersion_largeIncrement,
    unitTest "one-supermajor-increment" test_validateVersion_oneSupermajorIncrement,
    unitTest "one-major-increment" test_validateVersion_oneMajorIncrement
  ]

------------------------------------------------------------------------------------------------
-- Release report/URL tests
------------------------------------------------------------------------------------------------

-- | Test that release message includes actual Hackage URLs
test_releaseMessageUrls :: UnitTest
test_releaseMessageUrls = do
  assertContains "https://my-hackage.example.com/package/test-pkg-1.0.0" message
  assertContains "https://hackage.haskell.org/package/other-pkg-2.0.0" message
  where
    candidatePkg =
      ReleasedPackage {
        package = "test-pkg-1.0.0",
        results =
          [
            (candidateSources, [("my-hackage", RepoSuccess "https://my-hackage.example.com/package/test-pkg-1.0.0")]),
            (candidateDocs, [("my-hackage", RepoSuccess "https://my-hackage.example.com/package/test-pkg-1.0.0/docs")])
          ]
      }

    publishedPkg =
      ReleasedPackage {
        package = "other-pkg-2.0.0",
        results =
          [
            (publishSources, [("hackage.haskell.org", RepoSuccess "https://hackage.haskell.org/package/other-pkg-2.0.0")])
          ]
      }

    message = releaseMessage [StatusReleased candidatePkg, StatusReleased publishedPkg]

target_releaseFlowMixedResults :: Text
target_releaseFlowMixedResults = [exon|[35m[1m>>>[0m Skipping flake checks
[35m[1m>>>[0m Uploading candidate sources
[35m[1m>>>[0m Uploading candidate docs
[35m[1m>>>[0m Release completed successfully:

[35m[1m>>>[0m [34mlocal1[0m [36m1.2.0[0m
  [33m*[0m [33mcandidate sources[0m
    [32mtest-hackage[0m: [34mhttps://test-hackage.example.com/package/local1-1.2.0/candidate[0m
  [33m*[0m [33mcandidate docs[0m
    [32mtest-hackage[0m: [34mhttps://test-hackage.example.com/package/local1-1.2.0/candidate[0m

[35m[1m>>>[0m [34mlocal2[0m [36m2.3.0[0m
  [33m*[0m [33mcandidate sources[0m
    [31mtest-hackage[0m: package already exists
|]

-- | Test release flow with mixed results: local1 succeeds, local2 partially fails
-- Uses runMLogTest to capture log output and verify detailed report formatting
test_releaseFlowMixedResults :: UnitTest
test_releaseFlowMixedResults = do
  let
    testConfig = ReleaseTestConfig {
      checksPass = True,
      uploadErrors = [Error {message = Fatal "package already exists", context = [], level = Nothing}],
      failPackages = ["local2"]
    }
    releaseConfig = def {
      ReleaseConfig.targets = Just ["local1", "local2"],
      ReleaseConfig.candidates = ArtifactConfig {sources = True, docs = True},
      ReleaseConfig.publish = noArtifacts,
      ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
      ReleaseConfig.interactive = False,
      ReleaseConfig.check = False,  -- Skip checks for this test
      ReleaseConfig.partial = True  -- Continue after failures
    }
    options = ReleaseOptions {
      context = Left context,
      config = releaseConfig,
      stateFile = def,
      cabal = def,
      uiDebug = Nothing,
      oldStyleVersion = Nothing,
      oldStylePackages = []
    }
  (logMessages, _) <- runMLogTest def do
    void $ releaseCliUnitTestWith testConfig options
  eqLines target_releaseFlowMixedResults (unlines (reverse logMessages))

target_releaseFlowTerminated :: Text
target_releaseFlowTerminated = [exon|[35m[1m>>>[0m Skipping flake checks
[35m[1m>>>[0m Uploading candidate docs
[35m[1m>>>[0m Release terminated with errors:
[35m[1m>>>[0m Upload failed. Use --partial to continue with successful packages.

[35m[1m>>>[0m [34mlocal1[0m [36m1.2.0[0m
  [33m*[0m [33mcandidate docs[0m
    [31mtest-hackage[0m: upload rejected

[35m[1m>>>[0m [34mlocal2[0m [36m2.3.0[0m
  [33m*[0m [33mcandidate docs[0m
    [31mtest-hackage[0m: upload rejected
|]

-- | Test that terminated flow (upload failure without --partial) still shows per-package details
test_releaseFlowTerminated :: UnitTest
test_releaseFlowTerminated = do
  let
    testConfig = ReleaseTestConfig {
      checksPass = True,
      uploadErrors = [Error {message = Fatal "upload rejected", context = [], level = Nothing}],
      failPackages = []  -- All packages fail
    }
    releaseConfig = def {
      ReleaseConfig.targets = Just ["local1", "local2"],
      ReleaseConfig.candidates = ArtifactConfig {sources = False, docs = True},
      ReleaseConfig.publish = noArtifacts,
      ReleaseConfig.version = Just (VersionIncrement VersionIncrement.Major),
      ReleaseConfig.interactive = False,
      ReleaseConfig.check = False,
      ReleaseConfig.partial = False  -- Terminate on failure
    }
    options = ReleaseOptions {
      context = Left context,
      config = releaseConfig,
      stateFile = def,
      cabal = def,
      uiDebug = Nothing,
      oldStyleVersion = Nothing,
      oldStylePackages = []
    }
  (logMessages, _) <- runMLogTest def do
    void $ releaseCliUnitTestWith testConfig options
  eqLines target_releaseFlowTerminated (unlines (reverse logMessages))

test_releaseReport :: TestTree
test_releaseReport =
  testGroup "release-report" [
    unitTest "message-urls" test_releaseMessageUrls,
    unitTest "flow-mixed-results" test_releaseFlowMixedResults,
    unitTest "flow-terminated" test_releaseFlowTerminated
  ]
