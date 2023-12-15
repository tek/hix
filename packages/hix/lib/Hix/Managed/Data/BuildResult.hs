module Hix.Managed.Data.BuildResult where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Hix.Data.Bounds
import Hix.Data.Bounds (RemovableBounds (RemovableBounds))
import Hix.Data.ManagedEnv (ManagedState)
import qualified Hix.Data.PackageId
import Hix.Data.PackageName (PackageName)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.BuildState
import Hix.Managed.Data.BuildState (BuildState (BuildState), BuildSuccess (..), buildSuccessPackage, modifiedCandidates)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)

data BuildChanges f =
  BuildChanges {
    success :: f BuildSuccess,
    state :: ManagedState,
    removable :: Maybe RemovableBounds
  }
  deriving stock (Generic)

deriving stock instance Eq (f BuildSuccess) => Eq (BuildChanges f)
deriving stock instance Show (f BuildSuccess) => Show (BuildChanges f)

data BuildResult a =
  AllBuildsSucceeded (BuildChanges NonEmpty)
  |
  SomeBuildsFailed {
    changes :: BuildChanges [],
    failed :: NonEmpty (DepMutation a)
  }
  |
  NoActionRequired
  |
  FatalBuildFailure Text
  deriving stock (Eq, Show)

emptyBuildResult :: BuildResult a
emptyBuildResult =
  NoActionRequired

successes :: BuildResult a -> [BuildSuccess]
successes = \case
  AllBuildsSucceeded BuildChanges {success} -> toList success
  SomeBuildsFailed {changes = BuildChanges {success}} -> success
  _ -> []

successCandidates :: BuildResult a -> [Candidate]
successCandidates =
  modifiedCandidates . successes

unmodifiedDeps :: BuildResult a -> [PackageName]
unmodifiedDeps =
  successes >>> mapMaybe \case
    CandidateBuilt _ -> Nothing
    RangeUpdated _ _ -> Nothing
    Unmodified package -> Just package

failedMutations :: BuildResult a -> [DepMutation a]
failedMutations = \case
  SomeBuildsFailed {failed} -> toList failed
  _ -> []

buildResult ::
  RemovableBounds ->
  BuildState a s ->
  BuildResult a
buildResult RemovableBounds {targetBound, deps} BuildState {..} =
  case (nonEmpty (sortOn buildSuccessPackage success), nonEmpty (sortOn (.package) failed)) of
    (Just successNe, Nothing) ->
      AllBuildsSucceeded BuildChanges {success = successNe, ..}
    (_, Just failedNe) ->
      SomeBuildsFailed {changes = BuildChanges {..}, failed = failedNe}
    (Nothing, Nothing) ->
      NoActionRequired
  where
    removable = Just RemovableBounds {targetBound, deps = filterRemovable deps}
    filterRemovable =
      Map.mapMaybe \ targetDeps ->
        case Set.intersection targetDeps successNames of
          [] -> Nothing
          added -> Just added

    successNames = Set.fromList (modifiedCandidates success <&> (.package.name))

finalState :: BuildResult a -> Maybe ManagedState
finalState = \case
  AllBuildsSucceeded changes -> Just changes.state
  SomeBuildsFailed {changes} -> Just changes.state
  _ -> Nothing
