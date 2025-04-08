module Hix.Managed.StageResult where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty)

import Hix.Class.Map (nRestrictKeys)
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (failedMutation)
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState (MutationState))
import qualified Hix.Managed.Data.StageResult
import Hix.Managed.Data.StageResult (
  StageFailure (FailedMutations),
  StageResult (StageResult),
  StageSummary (StageFailure, StageNoAction, StageSuccess),
  )
import qualified Hix.Managed.Data.StageState
import Hix.Managed.Data.StageState (BuildSuccess, StageState (StageState))

stageResultWith ::
  Pretty a =>
  (Map MutableDep BuildSuccess -> MutationState -> MutationState) ->
  (Map MutableDep BuildSuccess -> Word -> Text) ->
  (Word -> Text) ->
  StageState a s ->
  StageResult
stageResultWith finalize successMessage failureMessage StageState {success, failed, state, iterations} =
  StageResult {state = Just (finalize success state), summary}
  where
    summary =
      case nonEmpty (sortOn (.package) failed) of
        Just failedNe -> StageFailure (FailedMutations (failureMessage iterations) (failedMutation <$> failedNe))
        Nothing | [] <- success -> StageNoAction (Just "All dependencies are up to date.")
                | otherwise -> StageSuccess (successMessage success iterations)

stageResult ::
  Pretty a =>
  (Map MutableDep BuildSuccess -> Word -> Text) ->
  (Word -> Text) ->
  StageState a s ->
  StageResult
stageResult =
  stageResultWith (const id)

stageResultInit ::
  Pretty a =>
  (Map MutableDep BuildSuccess -> Word -> Text) ->
  (Word -> Text) ->
  StageState a s ->
  StageResult
stageResultInit =
  stageResultWith \ success MutationState {..} ->
    MutationState {initial = nRestrictKeys (Map.keysSet success) versions, ..}
