module Hix.Managed.StageState where

import qualified Data.Map.Strict as Map

import Hix.Managed.Cabal.Data.SolverState (SolverState)
import qualified Hix.Managed.Data.MutableId
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (DepMutation, MutationResult (..))
import Hix.Managed.Data.StageState (BuildSuccess (CandidateBuilt, Unmodified), StageState (..))

updateStageState ::
  StageState a SolverState ->
  DepMutation a ->
  MutationResult ->
  StageState a SolverState
updateStageState old mutation = \case
  MutationSuccess {..} ->
    old {
      success = Map.insert candidate.name buildSuccess old.success,
      state,
      ext = solverState
    }
    where
      buildSuccess | changed = CandidateBuilt candidate
                   | otherwise = Unmodified mutation.package
  MutationKeep ->
    old {success = Map.insert mutation.package (Unmodified mutation.package) old.success}
  MutationFailed ->
    old {failed = mutation : old.failed}
