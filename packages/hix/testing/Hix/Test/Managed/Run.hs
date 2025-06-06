module Hix.Test.Managed.Run where

import Data.IORef (readIORef)
import Hedgehog (TestT, evalMaybe)

import Hix.Class.Map (nFromList, nKeys)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (LogLevel (..), M)
import Hix.Data.NixExpr (Expr)
import qualified Hix.Data.Options as ProjectOptions
import Hix.Data.Options (ProjectOptions)
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Versions)
import Hix.Data.VersionBounds (Bound (..))
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSynthetic))
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Data.BuildConfig (BuildConfig (toposortMutations))
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.ManagedPackage (ProjectPackages)
import Hix.Managed.Data.ProjectContext (ProjectContext)
import qualified Hix.Managed.Data.ProjectContextProto as ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Managed.Data.StageState (BuildStatus (Failure), resultFromStatus)
import qualified Hix.Managed.Handlers.Build as BuildHandlers
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import Hix.Managed.ProjectContext (processProjectResult, withProjectContext)
import Hix.Test.Run (LogConfig (..))
import Hix.Test.Utils (runMLogTest, runMTestLog)

data TestParams =
  TestParams {
    envs :: [(EnvName, (Maybe (Maybe Bound), [LocalPackage]))],
    cabalLog :: Bool,
    log :: Bool,
    logConfig :: LogConfig,
    packages :: ProjectPackages,
    ghcPackages :: GhcPackages,
    state :: ProjectStateProto,
    projectOptions :: ProjectOptions,
    build :: Versions -> M (BuildStatus, Overrides)
  }

nosortOptions :: ProjectOptions
nosortOptions = def {ProjectOptions.build = def {toposortMutations = False}}

testParams ::
  Bool ->
  ProjectPackages ->
  TestParams
testParams debug packages =
  TestParams {
    envs = [],
    cabalLog = False,
    log = False,
    logConfig = def {logLevel = if debug then LogDebug else LogError, onlyRef = not debug},
    packages,
    ghcPackages = GhcPackages {installed = [], available = []},
    state = def,
    projectOptions = nosortOptions,
    build = const (pure (Failure, mempty))
  }

withoutRevisions ::
  (Versions -> M BuildStatus) ->
  Versions ->
  M (BuildStatus, Overrides)
withoutRevisions build versions = do
  status <- build versions
  pure (status, mempty)

data Result a =
  Result {
    stateFile :: Expr,
    cabalLog :: [(EnvConstraints, Maybe SolverPlan)],
    log :: [Text],
    result :: a
  }

testProjectContext ::
  Bound ->
  TestParams ->
  ProjectContextProto
testProjectContext appBound params =
  ProjectContextProto {
    ProjectContextProto.packages = params.packages,
    state = params.state,
    envs = nFromList (second mkEnv <$> maybe defaultEnv toList (nonEmpty params.envs)),
    hackage = []
  }
  where
    mkEnv (bound, targets) =
      EnvConfig {
        targets,
        ghc = Just (GhcDbSynthetic params.ghcPackages),
        managedBound = fromMaybe (Just appBound) bound
      }

    defaultEnv = [(defaultEnvName, (Just (Just appBound), nKeys params.packages))]

    defaultEnvName = case appBound of
      BoundUpper -> "latest"
      BoundLower -> "lower"

managedTest' ::
  Bound ->
  TestParams ->
  (BuildHandlers -> ProjectOptions -> ProjectContextProto -> M a) ->
  TestT IO (Result a)
managedTest' appBound params main =
  withFrozenCallStack do
    (handlers0, stateFileRef, _) <-
      BuildHandlers.handlersUnitTest params.ghcPackages (fmap (first resultFromStatus) . params.build)
    (cabalRef, handlers1) <-
      if params.cabalLog
      then first Just <$> BuildHandlers.logCabal handlers0
      else pure (Nothing, handlers0)
    let
      handlers =
        if params.log
        then handlers1 {project = handlers1.project {report = ReportHandlers.handlersProd}}
        else handlers1
    (log, result) <- runner (main handlers params.projectOptions context)
    stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
    cabalLog <- fold <$> for cabalRef \ ref -> reverse <$> liftIO (readIORef ref)
    pure Result {..}
  where
    runner | params.log = runMLogTest params.logConfig
           | otherwise = fmap ([],) . runMTestLog params.logConfig

    context = testProjectContext appBound params

managedTest ::
  Bound ->
  TestParams ->
  (BuildHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO (Result ())
managedTest appBound params main =
  withFrozenCallStack do
    managedTest' appBound params \ handlers options context ->
      processProjectResult =<< withProjectContext appBound handlers.project options context (main handlers)

lowerTest ::
  TestParams ->
  (BuildHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO (Result ())
lowerTest params main =
  withFrozenCallStack do
    managedTest BoundLower params main

bumpTest ::
  TestParams ->
  (BuildHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO (Result ())
bumpTest params main =
  withFrozenCallStack do
    managedTest BoundUpper params main
