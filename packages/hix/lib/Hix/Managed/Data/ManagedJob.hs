module Hix.Managed.Data.ManagedJob where

import Hix.Data.Bounds (RemovableBounds)
import Hix.Data.Dep (Dep)
import Hix.Data.Deps (TargetDeps, TargetRemoteDeps)
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState (ManagedState))
import Hix.Data.Overrides (Overrides)
import Hix.Data.Version (Versions)
import Hix.Deps (mergeBounds)
import Hix.Managed.Data.Targets (Targets)

data ManagedJob =
  ManagedJob {
    -- | The name of the hix env used to build this job.
    env :: EnvName,
    -- | The set of packages this job operates on.
    -- Sorted topologically by smart constructor.
    targets :: Targets,

    -- TODO remove? also fix doc of 'removable' then
    -- | The bound this job is supposed to modify.
    -- targetBound :: TargetBound,

    -- | The flake deps with their ranges replaced by the managed bounds, if they exist.
    -- Invariant: keys are exactly the 'targets'.
    targetDeps :: TargetDeps,
    remoteDeps :: TargetRemoteDeps,
    lowerInit :: Versions,
    -- TODO in managedJob, filter these with the CLI options for selecting specific deps
    query :: [Dep],
    overrides :: Overrides,
    -- | The deps in each target that had a 'targetBound' in the flake config, but not in the managed state.
    -- For the purpose of informing the user that they can be removed.
    removable :: RemovableBounds
  }
  deriving stock (Eq, Show, Generic)

initialState :: ManagedJob -> ManagedState
initialState ManagedJob {remoteDeps, overrides} =
  ManagedState {bounds = mergeBounds remoteDeps, overrides}
