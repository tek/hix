module Hix.Test.Managed.Run where

import Data.IORef (readIORef)
import Hedgehog (TestT, evalMaybe)

import Hix.Class.Map (nFromList, nKeys)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (LogLevel (..), M)
import Hix.Data.NixExpr (Expr)
import qualified Hix.Data.Options as ProjectOptions
import Hix.Data.Options (ProjectOptions)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Versions)
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
    envs :: [(EnvName, [LocalPackage])],
    cabalLog :: Bool,
    log :: Bool,
    logConfig :: LogConfig,
    packages :: ProjectPackages,
    ghcPackages :: GhcPackages,
    state :: ProjectStateProto,
    projectOptions :: ProjectOptions,
    build :: Versions -> M BuildStatus
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
    build = const (pure Failure)
  }

data Result a =
  Result {
    stateFile :: Expr,
    cabalLog :: [(EnvConstraints, Maybe SolverPlan)],
    log :: [Text],
    result :: a
  }

testProjectContext ::
  EnvName ->
  TestParams ->
  ProjectContextProto
testProjectContext defaultEnvName params =
  ProjectContextProto {
    ProjectContextProto.packages = params.packages,
    state = params.state,
    envs = nFromList (second mkEnv <$> maybe defaultEnv toList (nonEmpty params.envs)),
    hackage = []
  }
  where
    mkEnv targets = EnvConfig {targets, ghc = Just (GhcDbSynthetic params.ghcPackages)}

    defaultEnv = [(defaultEnvName, nKeys params.packages)]

managedTest' ::
  EnvName ->
  TestParams ->
  (BuildHandlers -> ProjectOptions -> ProjectContextProto -> M a) ->
  TestT IO (Result a)
managedTest' defaultEnvName params main =
  withFrozenCallStack do
    (handlers0, stateFileRef, _) <-
      BuildHandlers.handlersUnitTest params.ghcPackages (fmap resultFromStatus . params.build)
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

    context = testProjectContext defaultEnvName params

managedTest ::
  EnvName ->
  TestParams ->
  (BuildHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO (Result ())
managedTest defaultEnvName params main =
  withFrozenCallStack do
    managedTest' defaultEnvName params \ handlers options context ->
      processProjectResult =<< withProjectContext handlers.project options context (main handlers)

lowerTest ::
  TestParams ->
  (BuildHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO (Result ())
lowerTest params main =
  withFrozenCallStack do
    managedTest "lower" params main

bumpTest ::
  TestParams ->
  (BuildHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO (Result ())
bumpTest params main =
  withFrozenCallStack do
    managedTest "latest" params main
