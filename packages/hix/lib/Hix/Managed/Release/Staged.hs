-- | Stage transitions for the release flow.
--
-- These functions convert release data between stages as the flow progresses.
module Hix.Managed.Release.Staged (
  -- * Existential wrappers
  ReleaseStage (..),
  -- * Stage transitions
  KnownStage,
  KnownFlowStatus,
  nextStage,
  transition,
  update,
  view,
  fromVersionChoice,
  excludeByPackage,
  toPrepared,
  toUploading,
  -- * Termination
  terminateStage,
  -- * Upload result updates
  updateUploadResult,
  -- * Queries
  currentSharedVersion,
) where

import Control.Lens ((%~))
import Data.Type.Equality ((:~:) (Refl))
import Exon (exon)
import Text.Show (ShowS)

import Hix.Class.Map (nAmend, nInsert, nMap, nMapMaybe, nPartition, (!?))
import Hix.Data.Error (Error)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.UploadResult (ArtifactResult)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseDist, ReleaseTarget (..))
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))
import Hix.Managed.Release.Data.Staged (
  FlowFailed,
  FlowStatus (..),
  FlowTargets,
  pattern InactiveIndividual,
  pattern InactiveShared,
  KnownFlowStatus (..),
  KnownStage (..),
  NextStage,
  ReleaseState (..),
  SFlowStatus (..),
  SStage (..),
  Stage (..),
  StageVersion (..),
  Target (..),
  TargetFailed (..),
  TargetPrepared (..),
  TargetUploading (..),
  Termination (..),
  Transition (..),
  VersionMode (..),
  )
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow (..))
import Hix.Managed.Release.Data.VersionChoice (SharedTarget (..), VersionChoice (..))

showStatus ::
  Show (FlowTargets status mode) =>
  Show (FlowFailed status mode) =>
  Int ->
  SFlowStatus status ->
  ReleaseState mode status ->
  ShowS
showStatus d tag state =
  showParen (d > 10) $
  [exon|ReleaseStage { tag = #{showsPrec 0 tag}, state = #{showsPrec 0 state} }|]

-- | Existential wrapper for release data at any status.
-- Hides both the version mode and flow status parameters.
data ReleaseStage where
  ReleaseStage ::
    ∀ mode status .
    {
      tag :: SFlowStatus status,
      state :: ReleaseState mode status
    } ->
    ReleaseStage

instance Show ReleaseStage where
  showsPrec d ReleaseStage {tag, state} = case tag of
    SInProgress SPristine -> showStatus d tag state
    SInProgress SSelecting -> showStatus d tag state
    SInProgress SPrepared -> showStatus d tag state
    SInProgress SUploading -> showStatus d tag state
    STerminated inner ->
      showParen (d > 10) $
      [exon|ReleaseStage { tag = #{showsPrec 0 tag}, terminated = #{showsPrec 0 inner} }|]

-- | Transition the release stage to 'Terminated', wrapping the current stage.
-- The data is preserved (type families dispatch through both flow status variants) and the
-- 'Termination' is set.
-- If the status is already terminated, the original is returned unchanged.
terminateStage :: TerminateFlow -> ReleaseStage -> ReleaseStage
terminateStage (TerminateFlow reason) = \case
  stage@ReleaseStage {tag = STerminated _} ->
    stage
  ReleaseStage {tag = SInProgress inner, state} ->
    ReleaseStage {tag = STerminated inner, state = state {termination = Termination reason}}

-- | Materialize evidence that the provided stage witness @target@ is the same as the current stage.
withMatchingStage ::
  ∀ target current a .
  SStage target ->
  SStage current ->
  a ->
  (KnownStage target => target :~: current -> a) ->
  a
withMatchingStage target c fallback f =
  check target c
  where
    check = \cases
      SPristine SPristine -> f Refl
      SSelecting SSelecting -> f Refl
      SPrepared SPrepared -> f Refl
      SUploading SUploading -> f Refl
      _ _ -> fallback


nextStage ::
  ∀ stage .
  KnownStage (NextStage stage) =>
  SStage stage ->
  (∀ mode . ReleaseState mode ('InProgress stage) -> M (Either TerminateFlow (ReleaseState mode ('InProgress (NextStage stage))))) ->
  ReleaseStage ->
  M ReleaseStage
nextStage from f stage@ReleaseStage {tag, ..} =
  case tag of
    SInProgress inner ->
      withMatchingStage from inner (pure stage) \ Refl ->
        f state <&> \case
          Right newState -> ReleaseStage {tag = SInProgress stageVal, state = newState}
          Left reason -> terminateStage reason stage
    STerminated _ -> pure stage

withTransition ::
  SFlowStatus from ->
  SFlowStatus to ->
  a ->
  (Transition from to -> a) ->
  a
withTransition from to fallback f =
  check from to
  where
    check = \cases
      (SInProgress SPristine) (SInProgress SSelecting) -> f PristineToSelecting
      (SInProgress SSelecting) (SInProgress SPrepared) -> f SelectingToPrepared
      (SInProgress SPrepared) (SInProgress SUploading) -> f PreparedToUploading
      (SInProgress SPristine) (STerminated SPristine) -> f (AnyToTerminate SPristine)
      (SInProgress SSelecting) (STerminated SSelecting) -> f (AnyToTerminate SSelecting)
      (SInProgress SPrepared) (STerminated SPrepared) -> f (AnyToTerminate SPrepared)
      (SInProgress SUploading) (STerminated SUploading) -> f (AnyToTerminate SUploading)
      _ _ -> fallback

transition ::
  ∀ to .
  KnownFlowStatus to =>
  SFlowStatus to ->
  (∀ mode from . Transition from to -> ReleaseState mode from -> M (Either TerminateFlow (ReleaseState mode to))) ->
  ReleaseStage ->
  M ReleaseStage
transition to f stage@ReleaseStage {tag = from, state} =
  withTransition from to (pure stage) \ trans ->
    f trans state <&> \case
      Right newState -> ReleaseStage {tag = flowStatusVal, state = newState}
      Left reason -> terminateStage reason stage

-- | Update the current status without transitioning.
-- If the current status doesn't match, the original data is returned unchanged.
--
-- If the callback returns @Left reason@, the stage is terminated with that reason.
update ::
  ∀ stage .
  SStage stage ->
  (∀ mode . ReleaseState mode ('InProgress stage) -> M (Either TerminateFlow (ReleaseState mode ('InProgress stage)))) ->
  ReleaseStage ->
  M ReleaseStage
update target f stage@ReleaseStage {tag, ..} =
  case tag of
    SInProgress inner ->
      withMatchingStage target inner (pure stage) \ Refl ->
        f state <&> \case
          Right newState -> ReleaseStage (SInProgress inner) newState
          Left reason -> terminateStage reason (ReleaseStage (SInProgress inner) state)
    STerminated _ -> pure stage

-- | Access the current stage data when in 'InProgress'.
view ::
  ∀ stage a .
  SStage stage ->
  (∀ mode . ReleaseState mode ('InProgress stage) -> a) ->
  ReleaseStage ->
  Maybe a
view target f ReleaseStage {tag, ..} =
  case tag of
    SInProgress inner ->
      withMatchingStage target inner Nothing \ Refl -> Just (f state)
    STerminated _ -> Nothing

-- | Convert a 'VersionChoice' to initial 'ReleaseState' wrapped existentially.
-- 'SharedVersion' creates @'ReleaseState' 'Shared' ('InProgress' 'Selecting')@.
-- 'IndividualVersions' creates @'ReleaseState' 'Individual' ('InProgress' 'Selecting')@.
fromVersionChoice :: VersionChoice -> ReleaseStage
fromVersionChoice = \case
  SharedVersion {version, sharedTargets, sharedExcluded} ->
    ReleaseStage @'Shared (SInProgress SSelecting) ReleaseState {
      version = Active version,
      targets = nMap toSharedTarget sharedTargets,
      failed = (),
      excluded = sharedExcluded,
      termination = NoTermination
    }
  IndividualVersions {individualTargets, individualExcluded} ->
    ReleaseStage @'Individual (SInProgress SSelecting) ReleaseState {
      version = InactiveShared,
      targets = nMap toIndividualTarget individualTargets,
      failed = (),
      excluded = individualExcluded,
      termination = NoTermination
    }
  where
    toSharedTarget :: SharedTarget -> Target 'Shared
    toSharedTarget SharedTarget {package, current} =
      Target {package, current, modeVersion = InactiveIndividual}

    toIndividualTarget :: ReleaseTarget -> Target 'Individual
    toIndividualTarget ReleaseTarget {package, current, version} =
      Target {package, current, modeVersion = Active version}

-- | Remove packages matching a predicate from the targets in the state.
excludeByPackage ::
  ∀ mode .
  (LocalPackage -> Bool) ->
  ReleaseState mode ('InProgress 'Selecting) ->
  ReleaseState mode ('InProgress 'Selecting)
excludeByPackage shouldExclude d@ReleaseState {targets} =
  let (newExcluded, kept) = nPartition toEither targets
  in d {targets = kept, excluded = newExcluded <> d.excluded}
  where
    toEither :: Target mode -> Either ConfiguredTarget (Target mode)
    toEither t@Target {package, current}
      | shouldExclude package = Left ConfiguredTarget {current, version = Nothing, selected = False}
      | otherwise = Right t

-- | Transition from 'Selecting' to 'Prepared' using the given Cabal dist tarballs or errors.
toPrepared ::
  ∀ mode .
  Packages (Either Error ReleaseDist) ->
  ReleaseState mode ('InProgress 'Selecting) ->
  ReleaseState mode ('InProgress 'Prepared)
toPrepared prepResults ReleaseState {version, targets, excluded} =
  ReleaseState {
    version,
    targets = nMapMaybe toPreparedTarget targets,
    failed = nMapMaybe toFailedTarget targets,
    excluded,
    termination = NoTermination
  }
  where
    toFailedTarget :: Target m -> Maybe (TargetFailed m)
    toFailedTarget t@Target {package} =
      case prepResults !? package of
        Just (Left err) -> Just TargetFailed {target = t, failure = err}
        _ -> Nothing

    toPreparedTarget :: Target m -> Maybe (TargetPrepared m)
    toPreparedTarget t@Target {package} =
      case prepResults !? package of
        Just (Right d) -> Just TargetPrepared {target = t, dist = d}
        _ -> Nothing

-- | Transition from 'Prepared' to 'Uploading' with empty upload stage results.
toUploading :: ∀ mode . ReleaseState mode ('InProgress 'Prepared) -> ReleaseState mode ('InProgress 'Uploading)
toUploading ReleaseState {version, targets, failed, excluded} =
  ReleaseState {
    version,
    targets = nMap toUploadingTarget targets,
    failed,
    excluded,
    termination = NoTermination
  }
  where
    toUploadingTarget :: TargetPrepared m -> TargetUploading m
    toUploadingTarget TargetPrepared {target, dist} =
      TargetUploading {target, dist, results = mempty}

-- | Update upload results for a specific stage across all packages.
updateUploadResult ::
  ∀ mode .
  UploadStage ->
  Packages ArtifactResult ->
  ReleaseState mode ('InProgress 'Uploading) ->
  ReleaseState mode ('InProgress 'Uploading)
updateUploadResult stage results =
  #targets %~ nAmend updateOne results
  where
    updateOne result = #results %~ nInsert stage result

-- | Extract shared version from any status.
currentSharedVersion :: ReleaseStage -> Maybe Version
currentSharedVersion ReleaseStage {tag, ..} = case tag of
  SInProgress SPristine -> Nothing
  SInProgress _ -> activeSelected state
  STerminated _ -> activeSelected state
  where
    activeSelected :: ReleaseState mode status -> Maybe Version
    activeSelected = \case
      ReleaseState {version = Active SelectedVersion {version}} -> Just version
      ReleaseState {version = Inactive _} -> Nothing

