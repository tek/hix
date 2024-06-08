module Hix.Managed.Data.StageState where

import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import Hix.Managed.Data.Mutation (DepMutation)
import Hix.Managed.Data.MutationState (MutationState)
import Hix.Data.PackageId (PackageId)

data BuildStatus =
  Success
  |
  Failure
  deriving stock (Eq, Show, Generic)

justSuccess :: a -> BuildStatus -> Maybe a
justSuccess a = \case
  Success -> Just a
  Failure -> Nothing

data BuildFailure =
  UnknownFailure
  |
  PackageFailure PackageId
  |
  TimeoutFailure (Maybe PackageId)
  deriving stock (Eq, Show, Generic)

data BuildResult =
  BuildSuccess
  |
  BuildFailure BuildFailure
  deriving stock (Eq, Show, Generic)

buildUnsuccessful :: BuildResult -> Bool
buildUnsuccessful = \case
  BuildSuccess -> False
  BuildFailure _ -> True

buildStatus :: BuildResult -> BuildStatus
buildStatus = \case
  BuildSuccess -> Success
  BuildFailure _ -> Failure

resultFromStatus :: BuildStatus -> BuildResult
resultFromStatus = \case
  Success -> BuildSuccess
  Failure -> BuildFailure UnknownFailure

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
