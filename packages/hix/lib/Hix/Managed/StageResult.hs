module Hix.Managed.StageResult where

import Distribution.Pretty (Pretty)

import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (failedMutation)
import qualified Hix.Managed.Data.StageResult
import Hix.Managed.Data.StageResult (
  StageFailure (FailedMutations),
  StageResult (StageResult),
  StageSummary (StageFailure, StageSuccess),
  )
import qualified Hix.Managed.Data.StageState
import Hix.Managed.Data.StageState (BuildSuccess, StageState (StageState))

stageResult ::
  Pretty a =>
  (Map MutableDep BuildSuccess -> Natural -> Text) ->
  (Natural -> Text) ->
  StageState a s ->
  StageResult
stageResult successMessage failureMessage StageState {success, failed, state, iterations} =
  StageResult {state = Just state, summary}
  where
    summary =
      case nonEmpty (sortOn (.package) failed) of
        Just failedNe -> StageFailure (FailedMutations (failureMessage iterations) (failedMutation <$> failedNe))
        Nothing -> StageSuccess (successMessage success iterations)
