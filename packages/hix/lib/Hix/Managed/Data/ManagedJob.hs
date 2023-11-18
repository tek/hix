module Hix.Managed.Data.ManagedJob where

import Hix.Data.Bounds (TargetBound)
import Hix.Data.Deps (TargetDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Managed.Data.Targets (Targets)

data ManagedJob =
  ManagedJob {
    -- | The name of the hix env used to build this job.
    env :: EnvName,
    -- | The set of packages this job operates on.
    -- By construction sorted topologically.
    targets :: Targets,
    -- | The bound this job is supposed to modify.
    targetBound :: TargetBound,
    -- | The flake deps with their ranges replaced by the managed bounds, if they exist.
    -- Invariant: keys are exactly the 'targets'.
    targetDeps :: TargetDeps
  }
  deriving stock (Eq, Show, Generic)
