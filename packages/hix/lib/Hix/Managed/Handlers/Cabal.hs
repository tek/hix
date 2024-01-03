module Hix.Managed.Handlers.Cabal where

import Data.IORef (IORef, modifyIORef')

import Hix.Class.Map (nFromKeys)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Data.Constraints (EnvConstraints)
import Hix.Managed.Data.Mutable (MutableDep, MutableVersions, depName)

data CabalHandlers =
  CabalHandlers {
    solveForVersion :: EnvConstraints -> M (Maybe SolverPlan),
    installedVersion :: PackageName -> Maybe Version
  }

handlersNull :: CabalHandlers
handlersNull =
  CabalHandlers {
    solveForVersion = \ _ -> pure Nothing,
    installedVersion = const Nothing
  }

installedVersions ::
  CabalHandlers ->
  Set MutableDep ->
  MutableVersions
installedVersions cabal names =
  nFromKeys names (cabal.installedVersion . depName)

logCabal :: IORef [(EnvConstraints, Maybe SolverPlan)] -> CabalHandlers -> CabalHandlers
logCabal ref CabalHandlers {..} =
  CabalHandlers {solveForVersion = solve, ..}
  where
    solve solverParams = do
      plan <- solveForVersion solverParams
      liftIO (modifyIORef' ref ((solverParams, plan) :))
      pure plan
