module Hix.Managed.Build.Single where

import Hix.Data.Monad (M)
import Hix.Data.VersionBounds (exactVersion)
import Hix.Managed.Build (buildConstraints)
import qualified Hix.Managed.Cabal.Data.SolverState
import Hix.Managed.Cabal.Data.SolverState (SolverState (SolverState), solverState)
import Hix.Managed.Constraints (fromVersions)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.Mutable (MutableVersions)
import Hix.Managed.Data.StageState (BuildStatus (Failure))
import Hix.Managed.Handlers.Build (EnvBuilder)
import Hix.Managed.Handlers.Hackage (HackageHandlers)

buildVersions ::
  HackageHandlers ->
  EnvBuilder ->
  EnvContext ->
  Text ->
  MutableVersions ->
  M BuildStatus
buildVersions handlers builder context description versions =
  buildConstraints handlers builder context description constraints <&> \case
    Just (_, _, status) -> status
    Nothing -> Failure
  where
    SolverState {constraints} = solverState context.solverBounds context.deps (fromVersions exactVersion versions)
