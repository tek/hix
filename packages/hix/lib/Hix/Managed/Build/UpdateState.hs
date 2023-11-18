module Hix.Managed.Build.UpdateState where

import Hix.Class.Map (ntInsert)
import Hix.Data.ManagedEnv (ManagedState (..))
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation, MutationResult (..))
import Hix.Managed.Data.Build (BuildState (..), BuildSuccess (CandidateBuilt, RangeUpdated, Unmodified))

updateState ::
  BuildState a s ->
  DepMutation a ->
  MutationResult s ->
  BuildState a s
updateState state mutation = \case
  MutationSuccess candidate managed ext ->
    BuildState {
      success = CandidateBuilt candidate : state.success,
      failed = state.failed,
      managed,
      ext
    }
  MutationUpdateBounds newVersion range ->
    state {
      success = RangeUpdated newVersion range : state.success,
      managed = state.managed {bounds = ntInsert mutation.package range state.managed.bounds}
    }
  MutationKeep ->
    state {
      success = Unmodified mutation.package : state.success
    }
  MutationFailed ->
    BuildState {
      success = state.success,
      failed = mutation : state.failed,
      managed = state.managed,
      ext = state.ext
    }
