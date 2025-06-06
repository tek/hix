module Hix.Managed.Build.Single where

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.VersionBounds (exactVersion)
import Hix.Managed.Build (buildConstraints)
import Hix.Managed.Cabal.Data.SolverState (solverState)
import Hix.Managed.Constraints (fromVersions)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.Initial (initial)
import Hix.Managed.Data.Mutable (MutableVersions)
import Hix.Managed.Data.StageState (BuildStatus (Failure))
import Hix.Managed.Handlers.Build (EnvBuilder)

-- | Passes 'False' to 'buildConstraints' to disable revisions, since this function is used to check whether the current
-- configuration builds successful, and revisions modify the set of overrides on failure.
buildVersions ::
  EnvBuilder ->
  EnvContext ->
  Text ->
  MutableVersions ->
  Maybe Overrides ->
  M BuildStatus
buildVersions builder context description versions prevOverrides =
  buildConstraints builder context description False (fold prevOverrides) (initial solver) <&> \case
    Just (_, _, status) -> status
    Nothing -> Failure
  where
    solver = solverState context.solverBounds context.deps (fromVersions exactVersion versions) def
