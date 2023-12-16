module Hix.Managed.Data.ManagedApp where

import Hix.Data.Bounds (Bounds)
import Hix.Data.ManagedEnv (ManagedEnvState)
import Hix.Managed.Data.ManagedConfig (ManagedOp, StateFileConfig)
import Hix.Managed.Data.ManagedJob (ManagedJob)
import Hix.Managed.Handlers.Build (BuildHandlers)

data ManagedApp =
  ManagedApp {
    build :: BuildHandlers,
    conf :: StateFileConfig,
    state :: ManagedEnvState,
    jobs :: NonEmpty ManagedJob,
    solverBounds :: Bounds,
    operation :: ManagedOp
  }