module Hix.Managed.Data.StageState where

import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (hang, vcat, (<+>), brackets)

import Hix.Data.Error (Error)
import Hix.Data.PackageId (PackageId)
import Hix.Managed.Build.NixOutput (PackageDerivation)
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import Hix.Managed.Data.Mutation (DepMutation)
import Hix.Managed.Data.MutationState (MutationState)
import Hix.Pretty (prettyL, prettyText)

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
  AppFailure Error
  |
  UnknownFailure
  |
  UnexpectedFailure (NonEmpty Text)
  |
  PackageFailure (NonEmpty PackageDerivation)
  |
  TimeoutFailure [PackageId]
  deriving stock (Eq, Show, Generic)

instance Pretty BuildFailure where
  pretty = \case
    AppFailure err -> "App failure:" <+> pretty err
    UnknownFailure -> "Unknown failure"
    UnexpectedFailure msgs -> hang "Unexpected failure:" 2 (vcat (prettyText <$> toList msgs))
    PackageFailure drvs -> "Package failures in" <+> prettyL drvs
    TimeoutFailure pkgs -> "Timeout failure, active builds:" <+> prettyL pkgs

data BuildResult =
  BuildSuccess [Text]
  |
  BuildFailure BuildFailure
  deriving stock (Eq, Show, Generic)

instance Pretty BuildResult where
  pretty = \case
    BuildSuccess [line] -> "Success" <+> brackets (prettyText line)
    BuildSuccess _ -> "Success"
    BuildFailure f -> pretty f

buildUnsuccessful :: BuildResult -> Bool
buildUnsuccessful = \case
  BuildSuccess _ -> False
  BuildFailure _ -> True

buildStatus :: BuildResult -> BuildStatus
buildStatus = \case
  BuildSuccess _ -> Success
  BuildFailure _ -> Failure

resultFromStatus :: BuildStatus -> BuildResult
resultFromStatus = \case
  Success -> BuildSuccess []
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
    revisions :: Set PackageId,
    iterations :: Natural,
    ext :: s
  }
  deriving stock (Eq, Show)

initStageState :: Initial MutationState -> s -> StageState a s
initStageState (Initial state) ext =
  StageState {success = [], failed = [], revisions = [], iterations = 0, ..}
