module Hix.Managed.Data.StageState where

import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import Hix.Managed.Data.Mutation (DepMutation)
import Hix.Managed.Data.MutationState (MutationState)

data BuildStatus =
  Success
  |
  Failure
  deriving stock (Eq, Show, Generic)

justSuccess :: a -> BuildStatus -> Maybe a
justSuccess a = \case
  Success -> Just a
  Failure -> Nothing

data BuildResult =
  Finished BuildStatus
  |
  TimedOut
  deriving stock (Eq, Show, Generic)

buildUnsuccessful :: BuildResult -> Bool
buildUnsuccessful = \case
  Finished Success -> False
  Finished Failure -> True
  TimedOut -> True

buildStatus :: BuildResult -> BuildStatus
buildStatus = \case
  Finished s -> s
  TimedOut -> Failure

data BuildSuccess =
  CandidateBuilt MutableId
  |
  Unmodified MutableDep
  deriving stock (Eq, Show, Generic)

buildSuccessPackage :: BuildSuccess -> MutableDep
buildSuccessPackage = \case
  CandidateBuilt MutableId {name} -> name
  Unmodified name -> name

modifiedCandidates :: [BuildSuccess] -> [MutableId]
modifiedCandidates =
  mapMaybe \case
    CandidateBuilt candidate -> Just candidate
    Unmodified _ -> Nothing

data StageState a s =
  StageState {
    success :: Map MutableDep BuildSuccess,
    failed :: [DepMutation a],
    state :: MutationState,
    iterations :: Natural,
    ext :: s
  }
  deriving stock (Eq, Show)

initStageState :: Initial MutationState -> s -> StageState a s
initStageState (Initial state) ext =
  StageState {success = [], failed = [], iterations = 0, ..}
