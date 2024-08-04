module Hix.Managed.Data.StageContext where

import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.Mutable (MutableBounds, MutableVersions)
import Hix.Managed.Data.MutationState (MutationState)
import Hix.Managed.Data.Query (Query)
import Hix.Managed.Handlers.Build (EnvBuilder)

data StageContext =
  StageContext {
    env :: EnvContext,
    builder :: EnvBuilder,
    state :: Initial MutationState,
    query :: Query,
    initialVersions :: MutableVersions,
    initialBounds :: MutableBounds
  }
