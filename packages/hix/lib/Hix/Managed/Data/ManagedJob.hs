module Hix.Managed.Data.ManagedJob where

import Hix.Data.Bounds (RemovableBounds)
import Hix.Data.Dep (Dep)
import Hix.Data.Deps (TargetDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Managed.Data.Targets (Targets)

data ManagedJob =
  ManagedJob {
    -- | The name of the hix env used to build this job.
    env :: EnvName,
    -- | The set of packages this job operates on.
    -- By construction sorted topologically.
    targets :: Targets,

    -- TODO remove? also fix doc of 'removable' then
    -- | The bound this job is supposed to modify.
    -- targetBound :: TargetBound,

    -- | The flake deps with their ranges replaced by the managed bounds, if they exist.
    -- Invariant: keys are exactly the 'targets'.
    targetDeps :: TargetDeps,
    deps :: [Dep],
    state :: ManagedState,
    -- | The deps in each target that had a 'targetBound' in the flake config, but not in the managed state.
    -- For the purpose of informing the user that they can be removed.
    removable :: RemovableBounds
  }
  deriving stock (Eq, Show, Generic)
