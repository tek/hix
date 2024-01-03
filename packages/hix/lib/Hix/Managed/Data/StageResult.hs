module Hix.Managed.Data.StageResult where

import Hix.Managed.Data.MutableId (MutableId)
import Hix.Managed.Data.Mutation (FailedMutation)
import Hix.Managed.Data.MutationState (MutationState)

data StageFailure =
  FailedPrecondition (NonEmpty Text)
  |
  FailedMutations Text (NonEmpty FailedMutation)
  deriving stock (Eq, Show, Generic)

data StageSummary =
  StageFailure StageFailure
  |
  StageSuccess Text
  |
  StageNoAction (Maybe Text)
  |
  StageReport Text (NonEmpty MutableId)
  deriving stock (Eq, Show, Generic)

data StageResult =
  StageResult {
    summary :: StageSummary,
    state :: Maybe MutationState
  }
  deriving stock (Eq, Show, Generic)

stageFailures :: StageSummary -> [FailedMutation]
stageFailures = \case
  StageFailure (FailedMutations _ mutations) -> toList mutations
  _ -> []
