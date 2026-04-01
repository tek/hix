-- | Release flow state management.
--
-- This module provides the 'ReleaseFlow' interface and state management for the release process.
-- The release progresses through four stages:
--
-- 1. @Pristine@: State is initialized empty
-- 2. @Selecting@: Package targets are configured with versions
-- 3. @Prepared@: Source distributions have been built
-- 4. @Uploading@: At least one upload attempt has been made
--
-- Stages are wrapped in 'FlowStatus': @InProgress@ or @Terminated@.
module Hix.Managed.Release.Flow where

import qualified Control.Monad.Trans.Class as MonadTrans
import Control.Monad.Trans.State.Strict (StateT (..), gets, modifyM)
import qualified Data.Set as Set
import Exon (exon)

import Hix.Class.Map (nAny, nMap, nMapMaybe, nTraverseWithKey, (!?))
import qualified Hix.Color as Color
import Hix.Data.Error (Error)
import Hix.Data.Monad (LogLevel (..), M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import qualified Hix.Log as Log
import Hix.Monad (shouldLog)
import Hix.Pretty (showP)
import Hix.Managed.Cabal.Data.UploadStage (ArtifactSort (..), UploadMutability (..), UploadStage (..))
import Hix.Managed.Data.Packages (Packages (..))
import Hix.Managed.Data.ReleaseConfig (ArtifactConfig (..), ReleaseConfig (..))
import Hix.Managed.Data.UploadResult (ArtifactResult, artifactResultFailed)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseDist)
import qualified Hix.Managed.Release.Data.Staged as Staged
import Hix.Managed.Release.Data.Staged (
  FlowStatus (..),
  KnownFlowStatus,
  NextStage,
  PreparedTargetView (..),
  ReleaseState (..),
  SFlowStatus (..),
  SStage (..),
  SelectedTargetView (..),
  Stage (..),
  TargetUploading (..),
  Transition (..),
  UploadingTargetView (..),
  emptyReleaseData,
  )
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow (..))
import Hix.Managed.Release.Data.UploadResult (hasFailure)
import Hix.Managed.Release.Data.VersionChoice (VersionChoice)
import qualified Hix.Managed.Release.Staged as Staged
import Hix.Managed.Release.Staged (KnownStage, ReleaseStage (..))

newtype ReleaseFlow a =
  ReleaseFlow (StateT ReleaseStage M a)
  deriving newtype (Functor, Applicative, Monad)

runReleaseFlow :: ReleaseFlow a -> M (a, ReleaseStage)
runReleaseFlow (ReleaseFlow f) =
  runStateT f ReleaseStage {tag = SInProgress SPristine, state = emptyReleaseData}

lift :: M a -> ReleaseFlow a
lift = ReleaseFlow . MonadTrans.lift

modify ::
  (ReleaseStage -> M ReleaseStage) ->
  ReleaseFlow ()
modify f =
  ReleaseFlow $ modifyM f

-- | Transition to the next stage if the current stage matches, with the option to terminate the flow if the callback
-- returns @Left reason@.
-- If the current stage doesn't match, nothing happens.
nextStage ::
  ∀ stage .
  KnownStage (NextStage stage) =>
  SStage stage ->
  (∀ mode . ReleaseState mode ('InProgress stage) -> M (Either TerminateFlow (ReleaseState mode ('InProgress (NextStage stage))))) ->
  ReleaseFlow ()
nextStage target f =
  modify (Staged.nextStage target f)

transition ::
  ∀ to .
  KnownFlowStatus to =>
  SFlowStatus to ->
  (∀ from mode . Transition from to -> ReleaseState mode from -> M (Either TerminateFlow (ReleaseState mode to))) ->
  ReleaseFlow ()
transition target f =
  modify (Staged.transition target f)

update ::
  ∀ stage .
  SStage stage ->
  (∀ mode . ReleaseState mode ('InProgress stage) -> M (Either TerminateFlow (ReleaseState mode ('InProgress stage)))) ->
  ReleaseFlow ()
update target f =
  modify (Staged.update target f)

viewStage ::
  ∀ stage a .
  SStage stage ->
  a ->
  (∀ mode . ReleaseState mode ('InProgress stage) -> a) ->
  ReleaseFlow a
viewStage target fallback f =
  ReleaseFlow $ gets \ s -> fromMaybe fallback (Staged.view target f s)

-- | Initialize the release targets from a 'VersionChoice'.
-- This stores the initial 'ReleaseState' in the 'Selecting' stage.
initTargets ::
  (Packages ConfiguredTarget -> M VersionChoice) ->
  Packages ConfiguredTarget ->
  ReleaseFlow ()
initTargets choose targets = do
  choice <- lift $ choose targets
  modify \ _ -> pure (Staged.fromVersionChoice choice)

-- | Get selected targets at the 'Selecting' stage (as views).
chosen :: ReleaseFlow (Packages SelectedTargetView)
chosen =
  viewStage SSelecting mempty Staged.selectedTargetViews

-- | Run an action with the selected targets and the shared version (if any).
withTargetsAndShared :: (Maybe Version -> Packages SelectedTargetView -> M a) -> ReleaseFlow a
withTargetsAndShared f = do
  shared <- getSharedVersion
  targets <- chosen
  lift (f shared targets)

-- | Transition from 'Selecting' to the 'Prepared' stage using a target choice UI and a stage handler.
initDists ::
  Bool ->
  (Bool -> Packages SelectedTargetView -> M (Either TerminateFlow (Set LocalPackage))) ->
  (SelectedTargetView -> M (Either Error ReleaseDist)) ->
  ReleaseFlow ()
initDists checksPassed chooseTargets buildDist =
  nextStage SSelecting \ selecting -> do
    let targets = Staged.selectedTargetViews selecting
    chooseTargets checksPassed targets >>= traverse \ selected -> do
      let selecting' = excludeUnselected selected selecting
      let selectedViews = Staged.selectedTargetViews selecting'
      stageResults <- nTraverseWithKey buildAndLog selectedViews
      pure (Staged.toPrepared stageResults selecting')
  where
    excludeUnselected selected =
      Staged.excludeByPackage (shouldExclude selected)

    shouldExclude selected pkg = not (Set.member pkg selected)

    buildAndLog pkg target = do
      Log.info [exon|Building release distributions for #{showP pkg}|]
      buildDist target

-- | Transition from 'Prepared' to 'Uploading'.
initUploading :: ReleaseFlow ()
initUploading =
  nextStage SPrepared \ prepared ->
    pure (Right (Staged.toUploading prepared))

-- | Determine whether the given stage is enabled by the CLI config.
-- This is controlled by the options @--candidates@ and @--publish@.
uploadStageEnabled :: ReleaseConfig -> UploadStage -> Bool
uploadStageEnabled config UploadStage {artifact, mutability} =
  matchArtifact (artifactConf mutability) artifact
  where
    artifactConf = \case
      UploadPublish -> config.publish
      UploadCandidate -> config.candidates

    matchArtifact ArtifactConfig {sources, docs} = \case
      ArtifactSources -> sources
      ArtifactDocs -> docs

-- | Transition the release stage to 'Terminated', preventing further stage transitions and updates.
terminate :: TerminateFlow -> ReleaseFlow ()
terminate reason =
  modify \ stage -> pure (Staged.terminateStage reason stage)

-- | Get uploading targets as views for UI display.
uploadingPackages :: ReleaseFlow (Packages UploadingTargetView)
uploadingPackages =
  viewStage SUploading mempty Staged.uploadingTargetViews

-- | Check whether a specific upload stage has any failures in the current state.
stageHasFailures :: UploadStage -> (∀ mode . ReleaseState mode ('InProgress 'Uploading) -> Bool)
stageHasFailures stage ReleaseState {targets} =
  nAny (checkTarget stage) targets
  where
    checkTarget s TargetUploading {results} =
      maybe False artifactResultFailed (results !? s)

-- | Check whether the last upload stage had failures and terminate if not in partial mode.
-- This runs as a separate step after 'update' to ensure the upload results are already recorded in the state
-- before termination.
checkPartialFailure ::
  ReleaseConfig ->
  UploadStage ->
  ReleaseFlow ()
checkPartialFailure config stage =
  unless config.partial do
    whenM (viewStage SUploading False (stageHasFailures stage)) do
      terminate "Upload failed. Use --partial to continue with successful packages."

-- | Execute an upload stage.
-- This handles:
-- 1. Checking if the stage is enabled by config
-- 2. Letting the user choose which packages to upload
-- 3. Running the uploads
-- 4. Updating the results in the state
-- 5. Terminating on failure if --partial wasn't specified
uploadStage ::
  ReleaseConfig ->
  UploadStage ->
  (UploadStage -> Packages PreparedTargetView -> M (Either TerminateFlow (Set LocalPackage))) ->
  (LocalPackage -> ReleaseDist -> M ArtifactResult) ->
  ReleaseFlow ()
uploadStage config tag chooseTargets uploadArtifact = do
  update SUploading \ uploading -> do
    let targets = toPreparedViews (Staged.uploadingTargetViews uploading)
    results <- if uploadStageEnabled config tag
    then do
      chooseTargets tag targets >>= traverse \ requested -> do
        nTraverseWithKey @_ @(Packages _) (runUpload requested) targets
    else pure (Right (nMap (const mempty) targets))
    pure (results <&> \ res -> Staged.updateUploadResult tag res uploading)
  checkPartialFailure config tag
  where
    toPreparedViews =
      nMapMaybe toPreparedView

    toPreparedView UploadingTargetView {package, current, releaseVersion, dist, results}
      | hasFailure results = Nothing
      | otherwise = Just PreparedTargetView {package, current, releaseVersion, dist}

    runUpload :: Set LocalPackage -> LocalPackage -> PreparedTargetView -> M ArtifactResult
    runUpload requested name PreparedTargetView {dist}
      | Set.member name requested = uploadArtifact name dist
      | otherwise = pure mempty

-- | Run the checks handler and log failures.
-- Only runs if the flow is in the 'Selecting' stage; returns @True@ (skip) otherwise.
checksStage ::
  M (Maybe [Text]) ->
  ReleaseFlow Bool
checksStage runChecks = do
  active <- viewStage SSelecting False (const True)
  if active
  then lift runChecks >>= \case
    Nothing -> pure True
    Just buildLog -> do
      lift (checksFailed buildLog)
      pure False
  else pure True

checksFailed :: [Text] -> M ()
checksFailed buildLog = do
  Log.error "The flake checks failed."
  ifM (shouldLog LogVerbose)
    do
      Log.verbose "Last 100 lines of output:"
      traverse_ Log.cont.verbose buildLog
    do
      let col = Color.shellCommand @Text
      Log.error [exon|Run #{col "nix -L flake check"} manually or re-run this command with #{col "--verbose"}|]

-- | Get the shared version if the user chose shared version mode.
getSharedVersion :: ReleaseFlow (Maybe Version)
getSharedVersion =
  ReleaseFlow $ gets Staged.currentSharedVersion

-- | Run an action with the uploading targets.
withUploading :: (Maybe Version -> Packages UploadingTargetView -> M a) -> ReleaseFlow a
withUploading f = do
  shared <- getSharedVersion
  uploading <- uploadingPackages
  lift (f shared uploading)
