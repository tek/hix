module Hix.Managed.Handlers.Mutation where

import Hix.Data.Monad (M)
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.Mutation (BuildMutation, DepMutation, MutationResult)
import Hix.Managed.Data.MutationState (MutationState)

data MutationHandlers a =
  MutationHandlers {
    process ::
      Initial SolverState ->
      DepMutation a ->
      (BuildMutation -> M (Maybe MutationState)) ->
      M MutationResult
  }
