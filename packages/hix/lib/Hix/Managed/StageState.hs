module Hix.Managed.StageState where

import qualified Data.Map.Strict as Map

import qualified Hix.Managed.Data.MutableId
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (DepMutation, MutationResult (..))
import Hix.Managed.Data.StageState (BuildSuccess (CandidateBuilt, Unmodified), StageState (..))

updateStageState ::
  StageState a s ->
  DepMutation a ->
  MutationResult s ->
  StageState a s
updateStageState old mutation = \case
  MutationSuccess candidate changed state ext ->
    old {
      success = Map.insert candidate.name buildSuccess old.success,
      state,
      ext
    }
    where
      buildSuccess | changed = CandidateBuilt candidate
                   | otherwise = Unmodified mutation.package
  MutationKeep ->
    old {success = Map.insert mutation.package (Unmodified mutation.package) old.success}
  MutationFailed ->
    old {failed = mutation : old.failed}
