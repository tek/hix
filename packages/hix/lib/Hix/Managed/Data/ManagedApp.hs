module Hix.Managed.Data.ManagedApp where

import Hix.Data.Bounds (Bounds)
import Hix.Data.ManagedEnv (ManagedEnvState)
import Hix.Managed.Data.ManagedConfig (ManagedOp, StateFileConfig)
import Hix.Managed.Data.ManagedJob (ManagedJob)
import Hix.Managed.Data.ManagedPackage (ManagedPackages)
import Hix.Managed.Handlers.Build (BuildHandlers)

data ManagedApp =
  ManagedApp {
    build :: BuildHandlers,
    conf :: StateFileConfig,
    state :: ManagedEnvState,
    packages :: ManagedPackages,
    jobs :: NonEmpty ManagedJob,
    solverBounds :: Bounds,
    operation :: ManagedOp
  }
