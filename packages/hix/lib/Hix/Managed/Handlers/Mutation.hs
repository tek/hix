module Hix.Managed.Handlers.Mutation where

import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import Hix.Managed.Build.Mutation (BuildMutation, DepMutation, MutationResult)

data MutationHandlers a s =
  MutationHandlers {
    process ::
      s ->
      DepMutation a ->
      (BuildMutation -> M (Maybe ManagedState)) ->
      M (MutationResult s)
  }
