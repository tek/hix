module Hix.Managed.Handlers.Lower.Test where

import Control.Lens (_1, (%~))
import Data.IORef (IORef)

import Hix.Data.ManagedEnv (BuildOutputsPrefix, EnvsConfig)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (Versions)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.BuildState (BuildStatus)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Lower (LowerHandlers (..), handlersNull)
import Hix.Managed.Handlers.Lower.Prod (handlersProd)
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers
import qualified Hix.Managed.Handlers.Solve.Test as SolveHandlers
import Hix.Managed.Lower.Data.Lower (Lower)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages, queryVersions, sourcePackageVersions)

handlersTest ::
  StateFileConfig ->
  EnvsConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  M LowerHandlers
handlersTest =
  handlersProd

handlersUnitTestNoSolver ::
  (Versions -> M BuildStatus) ->
  IO (LowerHandlers, IORef [Expr], IORef [DepMutation Lower])
handlersUnitTestNoSolver buildVersions = do
  (build, stateFileRef) <- BuildHandlers.handlersUnitTest buildVersions
  (report, mutationsRef) <- ReportHandlers.handlersUnitTest
  pure (handlersNull {build, report}, stateFileRef, mutationsRef)

handlersUnitTest ::
  (Versions -> M BuildStatus) ->
  [(PackageId, [PackageId])] ->
  SourcePackages ->
  IO (LowerHandlers, IORef [Expr], IORef [DepMutation Lower])
handlersUnitTest buildVersions installed available =
  (_1 %~ add) <$> handlersUnitTestNoSolver buildVersions
  where
    add h = h {solve, versions}
    solve _ = pure (SolveHandlers.testHandlers installed available)
    versions = queryVersions (sourcePackageVersions available)
