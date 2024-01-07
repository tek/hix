module Hix.Managed.Build.Single where

import Hix.Data.Monad (M)
import Hix.Data.VersionBounds (exactVersion)
import Hix.Managed.Build (buildConstraints)
import Hix.Managed.Cabal.Data.SolverState (solverState)
import Hix.Managed.Constraints (fromVersions)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.Mutable (MutableVersions)
import Hix.Managed.Data.StageState (BuildStatus (Failure))
import Hix.Managed.Handlers.Build (EnvBuilder)

buildVersions ::
  EnvBuilder ->
  EnvContext ->
  Text ->
  MutableVersions ->
  M BuildStatus
buildVersions builder context description versions =
  buildConstraints builder context description solver <&> \case
    Just (_, _, status) -> status
    Nothing -> Failure
  where
    solver = solverState context.solverBounds context.deps (fromVersions exactVersion versions) def
