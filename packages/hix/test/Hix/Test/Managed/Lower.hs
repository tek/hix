module Hix.Test.Managed.Lower where

import Data.IORef (IORef, readIORef)
import Hedgehog (TestT, evalEither, evalMaybe)

import Hix.Class.Map (nFromList, nKeys)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Data.Options (ProjectOptions)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSynthetic))
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectContext (ProjectContext)
import qualified Hix.Managed.Data.ProjectContextProto as ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Managed.Data.StageState (BuildStatus (Failure))
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Lower as LowerHandlers
import Hix.Managed.Handlers.Lower (LowerHandlers (..))
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import Hix.Managed.ProjectContext (withProjectContext)
import Hix.Test.Utils (runMLogTest, runMTest)

data LowerTestParams =
  LowerTestParams {
    envs :: [(EnvName, [LocalPackage])],
    cabalLog :: Bool,
    log :: Bool,
    debug :: Bool,
    packages :: Packages ManagedPackageProto,
    ghcPackages :: GhcPackages,
    state :: ProjectStateProto,
    projectOptions :: ProjectOptions,
    build :: Versions -> M BuildStatus
  }

lowerParams ::
  Bool ->
  Packages ManagedPackageProto ->
  LowerTestParams
lowerParams debug packages =
  LowerTestParams {
    envs = [],
    cabalLog = False,
    log = False,
    debug,
    packages,
    ghcPackages = GhcPackages {installed = [], available = []},
    state = def,
    projectOptions = def,
    build = const (pure Failure)
  }

data Result =
  Result {
    stateFile :: Expr,
    cabalLog :: [(EnvConstraints, Maybe SolverPlan)],
    log :: [Text]
  }

testProjectContext ::
  LowerTestParams ->
  ProjectContextProto
testProjectContext params =
  ProjectContextProto {
    ProjectContextProto.packages = params.packages,
    state = params.state,
    envs = nFromList (second mkEnv <$> maybe defaultEnv toList (nonEmpty params.envs)),
    buildOutputsPrefix = Nothing
  }
  where
    mkEnv targets = EnvConfig {targets, ghc = GhcDbSynthetic params.ghcPackages}

    defaultEnv = [("lower", nKeys params.packages)]

withCabalLog ::
  MonadIO m =>
  LowerHandlers ->
  m ((IORef [(EnvConstraints, Maybe SolverPlan)]), LowerHandlers)
withCabalLog handlers0 =
  LowerHandlers.logCabal handlers0 {LowerHandlers.build = handlers0.build {report = ReportHandlers.handlersProd}}

lowerTest ::
  LowerTestParams ->
  (LowerHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO Result
lowerTest params main =
  withFrozenCallStack do
    (handlers0, stateFileRef, _) <- LowerHandlers.handlersUnitTest params.build params.ghcPackages
    (cabalRef, handlers) <- if params.cabalLog then first Just <$> withCabalLog handlers0 else pure (Nothing, handlers0)
    (log, result) <- liftIO (runner (withProjectContext handlers.build params.projectOptions context (main handlers)))
    evalEither result
    stateFile <- evalMaybe . head =<< liftIO (readIORef stateFileRef)
    cabalLog <- fold <$> for cabalRef \ ref -> reverse <$> liftIO (readIORef ref)
    pure Result {..}
  where
    runner | params.log = runMLogTest params.debug (not params.debug)
           | otherwise = fmap ([],) . runMTest params.debug

    context = testProjectContext params
