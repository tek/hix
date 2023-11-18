module Hix.Managed.Build.UpdateState where

import Hix.Class.Map (ntInsert)
import Hix.Data.ManagedEnv (ManagedState (..))
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation, MutationResult (..))
import Hix.Managed.Data.Build (BuildState (..))

updateState ::
  BuildState a s ->
  DepMutation a ->
  MutationResult s ->
  BuildState a s
updateState state mutation = \case
  MutationSuccess candidate managed ext ->
    BuildState {
      success = candidate : state.success,
      failed = state.failed,
      managed,
      ext
    }
  MutationUpdateBounds range ->
    state {managed = state.managed {bounds = ntInsert mutation.package range state.managed.bounds}}
  MutationKeep -> state
  MutationFailed ->
    BuildState {
      success = state.success,
      failed = mutation : state.failed,
      managed = state.managed,
      ext = state.ext
    }
