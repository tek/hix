module Hix.Managed.Handlers.Release.Test where

import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef)
import GHC.IsList (fromList)
import Path (absfile)

import Hix.Data.Error (Error)
import Hix.Data.Monad (M)
import Hix.Data.Options (ReleaseOptions (..))
import Hix.Data.PackageName (LocalPackage, PackageName, localPackageName)
import Hix.Data.VersionBounds (Bound (BoundUpper))
import Hix.Http (httpManager)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish))
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Managed.Data.EnvConfig (EnvConfig (..))
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import Hix.Managed.Data.StateVersionsContext (StateVersionsContext (..))
import Hix.Managed.Data.UploadResult (ArtifactResult, RepoResult (..))
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.Handlers.Release (ReleaseHandlers (..))
import qualified Hix.Managed.Handlers.ReleaseUi.Batch as ReleaseUi
import qualified Hix.Managed.Handlers.ReleaseUi.Prod as ReleaseUi
import qualified Hix.Managed.Handlers.Upload.Prod as Upload
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseDist (..))
import Hix.Managed.Release.Data.Staged (SelectedTargetView)
import Hix.Managed.Release.Git (gitApiReleaseHermetic, gitApiReleaseUnitTest)
import Hix.Maybe (justIf)

-- | Data tracked by unit test handlers
data ReleaseTestData =
  ReleaseTestData {
    checksRun :: IORef Bool,
    uploadedArtifacts :: IORef [(Text, UploadStage, LocalPackage)]
  }

-- | Configuration for unit test handler behavior
data ReleaseTestConfig =
  ReleaseTestConfig {
    checksPass :: Bool,
    -- | Errors to return from @uploadArtifact@. When non-empty, each upload returns failures.
    uploadErrors :: [Error],
    -- | Packages that should fail. If empty, uploadErrors apply to all packages.
    failPackages :: [PackageName]
  }

defaultTestConfig :: ReleaseTestConfig
defaultTestConfig = ReleaseTestConfig {checksPass = True, uploadErrors = [], failPackages = []}

-- | Create fresh test data tracking structure
mkReleaseTestData :: IO ReleaseTestData
mkReleaseTestData = do
  checksRun <- newIORef False
  uploadedArtifacts <- newIORef []
  pure ReleaseTestData {checksRun, uploadedArtifacts}

-- | Unit test implementation of runChecks that records execution without running actual checks
runChecksUnitTest :: IORef Bool -> Bool -> M (Maybe [Text])
runChecksUnitTest checksRunRef result = do
  liftIO $ atomicWriteIORef checksRunRef True
  pure (justIf (not result) ["Checks failed"])

-- | Unit test implementation of uploadArtifact that records calls without performing actual upload.
--   Returns configured failures only for packages in failPackages (or all packages if failPackages is empty).
--   On success, returns mock URLs for the test Hackage servers.
--   Returns both successes and failures to test partial success scenarios.
uploadArtifactUnitTest ::
  ReleaseTestConfig ->
  IORef [(Text, UploadStage, LocalPackage)] ->
  Text ->
  UploadStage ->
  LocalPackage ->
  ReleaseDist ->
  M ArtifactResult
uploadArtifactUnitTest testConfig uploadedRef desc stage package _dist = do
  liftIO $ atomicModifyIORef' uploadedRef \xs -> ((desc, stage, package) : xs, ())
  pure results
  where
    shouldFail pkg
      | null testConfig.failPackages = not (null testConfig.uploadErrors)
      | localPackageName pkg `elem` testConfig.failPackages = True
      | otherwise = False

    results
      | shouldFail package
      , Just err <- head testConfig.uploadErrors
      = [("test-hackage", RepoFailed err)]
      | otherwise = [("test-hackage", RepoSuccess "https://test-hackage.example.com/package/")]

-- | Unit test implementation of releaseDist that returns mock paths without building
releaseDistUnitTest :: SelectedTargetView -> M (Either Error ReleaseDist)
releaseDistUnitTest _target =
  pure (Right ReleaseDist {sources = [absfile|/mock/sources.tar.gz|], docs = [absfile|/mock/docs.tar.gz|]})

-- | Construct 'ReleaseHandlers' with unit test implementations for testing.
--   Uses unit test implementations for 'runChecks' and 'uploadArtifact', keeps other handlers functional.
--   Returns the handlers and test data for making assertions.
handlersUnitTest ::
  ReleaseOptions ->
  CabalConfig ->
  M (ReleaseHandlers, ReleaseTestData)
handlersUnitTest = handlersUnitTestWith defaultTestConfig

-- | Construct 'ReleaseHandlers' with configurable test behavior.
handlersUnitTestWith ::
  ReleaseTestConfig ->
  ReleaseOptions ->
  CabalConfig ->
  M (ReleaseHandlers, ReleaseTestData)
handlersUnitTestWith testConfig options cabal = do
  testData <- liftIO mkReleaseTestData
  manager <- httpManager
  project <- Project.handlersProd options.stateFile
  upload <- Upload.handlersProd False cabal
  publishHackages <- HackageClient.handlersProdFor (Just manager) ForPublish cabal
  let
    handlers = ReleaseHandlers {
      runChecks = runChecksUnitTest testData.checksRun testConfig.checksPass,
      releaseDist = releaseDistUnitTest,
      uploadArtifact = uploadArtifactUnitTest testConfig testData.uploadedArtifacts,
      git = const gitApiReleaseUnitTest,
      context = contextHandlersUnitTest,
      project,
      upload,
      publishHackages,
      ui
    }
  pure (handlers, testData)
  where
    contextHandlersUnitTest = ContextHandlers.handlersTest \case
      ContextHandlers.ContextQuery ContextHandlers.ContextManaged -> pure (Just managedContext)
      ContextHandlers.ContextQuery ContextHandlers.ContextStateVersions -> pure (Just stateVersionsContext)
      _ -> pure Nothing

    stateVersionsContext = StateVersionsContext {
      state = def,
      versionFile = Nothing,
      packages = mempty
    }

    managedContext = def {
      envs = fromList [("test", envConfig)]
    }

    envConfig = EnvConfig {
      targets = [],
      ghc = Nothing,
      managedBound = Just BoundUpper
    }

    ui =
      if options.config.interactive
      then ReleaseUi.handlersProdWithEvents options.uiDebug
      else ReleaseUi.handlersBatch options.config

-- | Construct ReleaseHandlers with real git operations but mock uploads.
-- Used for testing git workflows end-to-end.
handlersGitTest ::
  ReleaseTestConfig ->
  ReleaseOptions ->
  CabalConfig ->
  M (ReleaseHandlers, ReleaseTestData)
handlersGitTest testConfig options cabal = do
  testData <- liftIO mkReleaseTestData
  manager <- httpManager
  project <- Project.handlersProd options.stateFile
  upload <- Upload.handlersProd False cabal
  publishHackages <- HackageClient.handlersProdFor (Just manager) ForPublish cabal
  let
    handlers = ReleaseHandlers {
      runChecks = runChecksUnitTest testData.checksRun testConfig.checksPass,
      releaseDist = releaseDistUnitTest,
      uploadArtifact = uploadArtifactUnitTest testConfig testData.uploadedArtifacts,
      -- Use real hermetic git handlers for testing git workflows
      git = gitApiReleaseHermetic options.config,
      context = contextHandlersUnitTest,
      project,
      upload,
      publishHackages,
      ui
    }
  pure (handlers, testData)
  where
    contextHandlersUnitTest = ContextHandlers.handlersTest \case
      ContextHandlers.ContextQuery ContextHandlers.ContextManaged -> pure (Just managedContext)
      ContextHandlers.ContextQuery ContextHandlers.ContextStateVersions -> pure (Just stateVersionsContext)
      _ -> pure Nothing

    stateVersionsContext = StateVersionsContext {
      state = def,
      versionFile = Nothing,
      packages = mempty
    }

    managedContext = def {
      envs = fromList [("test", envConfig)]
    }

    envConfig = EnvConfig {
      targets = [],
      ghc = Nothing,
      managedBound = Just BoundUpper
    }

    ui =
      if options.config.interactive
      then ReleaseUi.handlersProdWithEvents options.uiDebug
      else ReleaseUi.handlersBatch options.config
