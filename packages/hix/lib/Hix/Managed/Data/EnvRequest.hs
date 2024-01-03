module Hix.Managed.Data.EnvRequest where

import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Handlers.Build (EnvBuilder)

-- | Resources for an env job.
data EnvRequest =
  EnvRequest {
    -- | For convenience.
    context :: EnvContext,

    builder :: EnvBuilder,

    -- | The state at the beginning of the processing step for this env.
    state :: Initial EnvState
  }
