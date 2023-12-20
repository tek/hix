module Hix.Managed.Data.ManagedJob where

import Hix.Data.Bounds (RemovableBounds)
import Hix.Data.Deps (TargetDeps)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState (ManagedState))
import Hix.Data.Overrides (Overrides)
import Hix.Data.Version (Versions)
import Hix.Deps (mergeBounds)
import qualified Hix.Managed.Data.BuildDomain
import Hix.Managed.Data.BuildDomain (BuildDomain (BuildDomain))

data ManagedJob =
  ManagedJob {
    domain :: BuildDomain,

    -- TODO remove? also fix doc of 'removable' then
    -- | The bound this job is supposed to modify.
    -- targetBound :: TargetBound,

    -- | The flake deps with their ranges replaced by the managed bounds, if they exist.
    -- Invariant: keys are exactly the 'targets'.
    targetDeps :: TargetDeps,
    lowerInit :: Versions,
    overrides :: Overrides,
    -- | The deps in each target that had a 'targetBound' in the flake config, but not in the managed state.
    -- For the purpose of informing the user that they can be removed.
    --
    -- TODO this should be Init-only
    removable :: RemovableBounds
  }
  deriving stock (Eq, Show, Generic)

initialState :: ManagedJob -> ManagedState
initialState ManagedJob {domain = BuildDomain {deps}, overrides} =
  ManagedState {bounds = mergeBounds deps, overrides}
