-- | Release report formatting for detailed upload status reporting.
--
-- This module provides functions to generate human-readable reports showing which upload stages succeeded or failed for
-- each package, including per-server status for multi-server uploads.
module Hix.Managed.Release.Report (
  report,
) where


import Exon (exon)
import Text.PrettyPrint (Doc)

import Hix.Class.Map (nConcat, nNull, nTo)
import Hix.Color (blue, cyan, green, red, yellow)
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage, PackageName (..), localPackageName)
import Hix.Data.Version (Version)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.UploadResult (ArtifactResult, HackageUrl (..), RepoResult (..))
import Hix.Managed.Handlers.Report.Prod (blankLine)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget)
import Hix.Managed.Release.Data.ReleaseResult (
  FailedPackage (..),
  PackageStatus (..),
  ReleasedPackage (..),
  SkippedPackage (..),
  )
import Hix.Managed.Release.Data.Staged (
  FlowStatus (..),
  ReleaseState (..),
  SFlowStatus (..),
  SStage (..),
  Stage (..),
  StageVersion (..),
  Target (..),
  TargetFailed (..),
  TargetPrepared (..),
  Termination (..),
  VersionMode (..),
  modeReleaseVersion,
  )
import Hix.Managed.Release.ReleaseResult (errorMessage, releaseStatusesFrom)
import Hix.Managed.Release.Staged (ReleaseStage (..))

indent :: [Text] -> [Text]
indent = fmap ("  " <>)

bullet :: Text
bullet = yellow @Doc "*"

withBullet :: Text -> Text
withBullet l = [exon|#{bullet} #{l}|]

-- | Format a single repo's outcome for an artifact.
formatRepoResult :: HackageName -> RepoResult -> [Text]
formatRepoResult repo result = case result of
  RepoSuccess (HackageUrl url) -> [[exon|#{green repo}: #{blue url}|]]
  RepoFailed err -> [[exon|#{red repo}: #{errorMessage err}|]]

-- | Format artifact results with label.
formatArtifactDetails :: UploadStage -> ArtifactResult -> [Text]
formatArtifactDetails label artifact =
  if nNull artifact
  then []
  else indent (withBullet [exon|#{yellow label}|] : indent (nConcat artifact formatRepoResult))

reportReleased :: ReleasedPackage -> M ()
reportReleased pkg@ReleasedPackage {package = PackageId {name = PackageName pkgName, version}} = do
  Log.info [exon|#{blue pkgName} #{cyan version}|]
  traverse_ Log.plain.info (nConcat pkg.results formatArtifactDetails)

reportFailed :: FailedPackage -> M ()
reportFailed FailedPackage {package, reason} =
  Log.info [exon|#{blue package} #{red ("failed" :: Text)} #{reason}|]

reportSkipped :: SkippedPackage -> M ()
reportSkipped SkippedPackage {name} =
  Log.info [exon|#{blue name} #{yellow ("skipped" :: Text)}|]

reportStatus :: PackageStatus -> M ()
reportStatus = \case
  StatusReleased pkg -> reportReleased pkg
  StatusFailed pkg -> reportFailed pkg
  StatusSkipped pkg -> reportSkipped pkg

-- | Report per-package upload results for the 'Uploading' stage.
reportUploadingResults :: ReleaseState mode ('InProgress 'Uploading) -> M ()
reportUploadingResults state =
  traverse_ reportPackage (releaseStatusesFrom state.version state.targets state.failed state.excluded)
  where
    reportPackage status = do
      blankLine
      reportStatus status

-- | Report a package name with its version.
reportPackageLine :: LocalPackage -> Version -> M ()
reportPackageLine name version = do
  let PackageName pkgName = localPackageName name
  Log.info [exon|#{blue pkgName} #{cyan version}|]

-- | Report a package that failed with an error.
reportPackageFailure :: LocalPackage -> Version -> Text -> M ()
reportPackageFailure name version reason = do
  let PackageName pkgName = localPackageName name
  Log.info [exon|#{blue pkgName} #{cyan version} #{red ("failed" :: Text)} #{reason}|]

-- | Report excluded packages.
reportExcluded :: Packages ConfiguredTarget -> M ()
reportExcluded excluded =
  for_ (nTo excluded toSkipped) \ name -> do
    blankLine
    reportSkipped (SkippedPackage {name})
  where
    toSkipped name _ = name

-- | Report selected targets at the 'Selecting' stage.
reportSelectingResults :: ∀ mode . StageVersion 'Shared mode -> ReleaseState mode ('InProgress 'Selecting) -> M ()
reportSelectingResults sv ReleaseState {targets, excluded} = do
  for_ (nTo targets toLine) \ (name, version) -> do
    blankLine
    reportPackageLine name version
  reportExcluded excluded
  where
    toLine name Target {modeVersion} =
      (name, modeReleaseVersion sv modeVersion)

-- | Report results at the 'Prepared' stage, showing prepared and failed targets.
reportPreparedResults :: ∀ mode . StageVersion 'Shared mode -> ReleaseState mode ('InProgress 'Prepared) -> M ()
reportPreparedResults sv ReleaseState {targets, failed, excluded} = do
  for_ (nTo targets toPreparedLine) \ (name, version) -> do
    blankLine
    reportPackageLine name version
  for_ (nTo failed toFailedLine) \ (name, version, reason) -> do
    blankLine
    reportPackageFailure name version reason
  reportExcluded excluded
  where
    toPreparedLine name TargetPrepared {target = Target {modeVersion}} =
      (name, modeReleaseVersion sv modeVersion)

    toFailedLine name TargetFailed {target = Target {modeVersion}, failure} =
      (name, modeReleaseVersion sv modeVersion, errorMessage failure)

-- | Report the content for a specific inner stage.
reportStageResults :: SStage stage -> ReleaseState mode ('InProgress stage) -> M ()
reportStageResults = \case
  SUploading -> reportUploadingResults
  SPristine -> \ _ -> unit
  SSelecting -> \ state -> reportSelectingResults state.version state
  SPrepared -> \ state -> reportPreparedResults state.version state

reportSuccess :: ReleaseState mode ('InProgress 'Uploading) -> M ()
reportSuccess state = do
  Log.info "Release completed successfully:"
  reportStageResults SUploading state

reportTermination :: SStage s -> ReleaseState mode ('Terminated s) -> M ()
reportTermination final ReleaseState {termination = Termination reason, version, targets, failed, excluded} = do
  Log.info "Release terminated with errors:"
  Log.info reason
  -- Reconstruct as InProgress for stage-specific reporting, since the data is structurally identical
  -- (FlowTargets and FlowFailed dispatch to the same types for both InProgress and Terminated).
  reportStageResults final ReleaseState {termination = NoTermination, version, targets, failed, excluded}

-- | Report packages involved at pre-upload stages where no upload data exists.
reportEarlyTermination :: Text -> SStage stage -> ReleaseState mode ('InProgress stage) -> M ()
reportEarlyTermination desc stage state = do
  Log.info [exon|Release terminated #{desc}.|]
  reportStageResults stage state

reportFlowStatus :: SFlowStatus status -> ReleaseState mode status -> M ()
reportFlowStatus = \case
  STerminated final -> reportTermination final
  SInProgress SPristine -> reportEarlyTermination "before version selection" SPristine
  SInProgress SSelecting -> reportEarlyTermination "during version selection" SSelecting
  SInProgress SPrepared -> reportEarlyTermination "during preparation" SPrepared
  SInProgress SUploading -> reportSuccess

-- | Print a summary report of all release results.
-- If the release was terminated early, the termination reason is shown.
-- Uses the stage-specific reporters to display per-package details even when the flow was aborted.
report :: ReleaseStage -> M ()
report ReleaseStage {tag, state} = reportFlowStatus tag state
