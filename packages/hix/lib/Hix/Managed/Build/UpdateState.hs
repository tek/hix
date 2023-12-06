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
updateState old mutation = \case
  MutationSuccess candidate managed ext ->
    old {
      success = CandidateBuilt candidate : old.success,
      state = managed,
      ext
    }
  MutationUpdateBounds newVersion range ->
    old {
      success = RangeUpdated newVersion range : old.success,
      state = old.state {bounds = ntInsert mutation.package range old.state.bounds}
    }
  MutationKeep ->
    old {success = Unmodified mutation.package : old.success}
  MutationFailed ->
    old {failed = mutation : old.failed}
