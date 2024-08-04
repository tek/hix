module Hix.Managed.Maint.Data.MaintEnv where

import Hix.Managed.Data.MaintConfig (MaintConfig)
import Hix.Managed.Handlers.Maint (MaintHandlers)
import Hix.Managed.Maint.Data.MaintTarget (MaintTarget)
import Hix.Managed.Maint.Git (GitMaint)

data MaintEnv =
  MaintEnv {
    target :: MaintTarget,
    handlers :: MaintHandlers,
    git :: GitMaint,
    config :: MaintConfig
  }
