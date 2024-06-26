module Hix.Managed.Handlers.Mutation where

import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId)
import Hix.Managed.Data.Mutation (BuildMutation, DepMutation, MutationResult)
import Hix.Managed.Data.MutationState (MutationState)

data MutationHandlers a s =
  MutationHandlers {
    process ::
      s ->
      DepMutation a ->
      (BuildMutation -> M (Maybe (MutationState, Set PackageId))) ->
      M (MutationResult s)
  }
