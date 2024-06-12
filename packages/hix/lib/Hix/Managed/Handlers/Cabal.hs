module Hix.Managed.Handlers.Cabal where

import Data.IORef (IORef, modifyIORef')
import Distribution.PackageDescription (PackageDescription)

import Hix.Class.Map (nFromKeys)
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import Hix.Managed.Data.Constraints (EnvConstraints)
import Hix.Managed.Data.Mutable (MutableDep, MutableVersions, depName)
import Hix.Managed.Data.Mutation (DepMutation)

data CabalHandlers =
  CabalHandlers {
    solveForVersion :: SolverState -> M (Maybe SolverPlan),
    installedVersion :: PackageName -> Maybe Version,
    sourcePackage :: PackageId -> Maybe PackageDescription,
    sortMutations :: ∀ a . [DepMutation a] -> M [DepMutation a]
  }

handlersNull :: CabalHandlers
handlersNull =
  CabalHandlers {
    solveForVersion = \ _ -> pure Nothing,
    installedVersion = const Nothing,
    sourcePackage = const Nothing,
    sortMutations = pure
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
    solve state = do
      plan <- solveForVersion state
      liftIO (modifyIORef' ref ((state.constraints, plan) :))
      pure plan
