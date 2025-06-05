module Hix.Managed.Release.ReleaseResult where

import Data.Char (toUpper)
import qualified Data.Text as Text
import Distribution.Version (Version)
import Exon (exon)
import GHC.IsList ()

import Hix.Class.Map (nConcat, nMap, nMapWithKey, nNull, nTo)
import qualified Hix.Color as Color
import Hix.Data.Error (Error (..), ErrorMessage (..))
import Hix.Data.Monad (M)
import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget)
import Hix.Data.PackageId (PackageId (..), renderPackage)
import Hix.Data.PackageName (LocalPackage (..), PackageName (..), localPackageName)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName (..))
import Hix.Managed.Cabal.Data.UploadStage (UploadMutability (..), UploadStage (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.UploadResult (ArtifactResult, HackageUrl (..), RepoResult (..))
import Hix.Managed.Git (BranchName)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget)
import Hix.Managed.Release.Data.ReleaseResult (
  FailedPackage (..),
  PackageStatus (..),
  ReleaseOutput (..),
  ReleasedPackage (..),
  SkippedPackage (..),
  filterReleased,
  isFailure,
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
  TargetUploading (..),
  VersionMode (..),
  modeReleaseVersion,
  )
import Hix.Managed.Release.Data.UploadResult (PackageResult)
import Hix.Managed.Release.Staged (ReleaseStage (..))
import Hix.Monad (clientError)
import Hix.OutputWriter (OutputSpecial (..), writeOutput)
import Hix.Pretty (showP)

formatSuccessUrl :: UploadMutability -> PackageId -> HackageUrl -> HackageUrl
formatSuccessUrl mutability package baseUrl =
  HackageUrl [exon|##{baseUrl}#{renderPackage package}#{suffix}|]
  where
    suffix = case mutability of
      UploadCandidate -> "/candidate"
      UploadPublish -> ""

formatRepoResult :: UploadMutability -> PackageId -> RepoResult -> RepoResult
formatRepoResult mutability package = \case
  RepoSuccess url -> RepoSuccess (formatSuccessUrl mutability package url)
  err@RepoFailed {} -> err

buildReleasedPackage :: PackageId -> PackageResult -> ReleasedPackage
buildReleasedPackage package results =
  ReleasedPackage {
    package,
    results = nMapWithKey (formatArtifact package) results
  }
  where
    formatArtifact :: PackageId -> UploadStage -> ArtifactResult -> ArtifactResult
    formatArtifact pkg UploadStage {mutability} =
      nMap (formatRepoResult mutability pkg)

-- | Format artifact results for markdown display.
formatArtifactResult :: Text -> ArtifactResult -> [Text]
formatArtifactResult label artifact =
  if nNull artifact
  then []
  else [exon|**#{label}**|] : formatResults
  where
    formatResults =
      nConcat artifact \ repo result ->
        [[exon|  * #{coerce @_ @Text repo}: #{formatResult result}|]]

    formatResult = \case
      RepoSuccess (HackageUrl url) -> [exon|[#{url}](#{url})|]
      RepoFailed err -> [exon|**failed** - #{errorMessage err}|]

packageStatusName :: PackageStatus -> Text
packageStatusName = \case
  StatusReleased ReleasedPackage {package} -> coerce package.name
  StatusFailed FailedPackage {package} -> coerce package.name
  StatusSkipped SkippedPackage {name} -> coerce name

-- | Generate a markdown PR body grouped by package.
releaseMessage :: [PackageStatus] -> Text
releaseMessage statuses =
  Text.intercalate "\n" (concatMap formatPackageStatus (sortOn packageStatusName statuses))
  where
    formatPackageStatus :: PackageStatus -> [Text]
    formatPackageStatus = \case
      StatusReleased pkg -> formatReleased pkg
      StatusFailed pkg -> formatFailed pkg
      StatusSkipped pkg -> formatSkippedPkg pkg

    formatReleased :: ReleasedPackage -> [Text]
    formatReleased pkg@ReleasedPackage {package} =
      [exon|### #{renderPackage package}|] :
      "" :
      nConcat pkg.results \ stage artifact -> formatArtifactResult (capitalizeLabel (showP stage)) artifact

    capitalizeLabel :: Text -> Text
    capitalizeLabel t = case Text.uncons t of
      Just (c, rest) -> Text.cons (toUpper c) rest
      Nothing -> t

    formatFailed :: FailedPackage -> [Text]
    formatFailed FailedPackage {package, reason} =
      [
        [exon|### #{coerce @_ @Text package.name}|],
        "",
        [exon|**Failed**: #{renderPackage package}|],
        "",
        reason
      ]

    formatSkippedPkg :: SkippedPackage -> [Text]
    formatSkippedPkg SkippedPackage {name} =
      [
        [exon|### ##{name}|],
        "",
        "*Skipped*"
      ]

errorMessage :: Error -> Text
errorMessage Error {message} = case message of
  Fatal msg -> msg
  FatalExternal msg -> msg
  Client msg -> msg

-- | Generate a PR title.
-- If a shared version is provided, uses "Release <version>".
-- Otherwise, lists each package-version pair.
releaseTitle ::
  [PackageStatus] ->
  Maybe Version ->
  Text
releaseTitle statuses = \case
  Just v -> [exon|Release #{showP v}|]
  Nothing -> case filterReleased statuses of
    [] -> "Release (no packages)"
    packages -> [exon|Release #{packageList packages}|]
  where
    packageList :: [ReleasedPackage] -> Text
    packageList = Text.intercalate ", " . fmap \ p -> renderPackage p.package

-- | Build release statuses from decomposed 'ReleaseState' fields.
-- This is used by 'viewInner' to access upload results through the 'Terminated' wrapper.
releaseStatusesFrom ::
  ∀ mode .
  StageVersion 'Shared mode ->
  Packages (TargetUploading mode) ->
  Packages (TargetFailed mode) ->
  Packages ConfiguredTarget ->
  [PackageStatus]
releaseStatusesFrom sv targets failed excluded =
  nTo targets toUploadingStatus
  <>
  nTo failed toFailedPackage
  <>
  nTo excluded toSkipped
  where
    toUploadingStatus name TargetUploading {target = Target {modeVersion}, results} =
      let ver = modeReleaseVersion sv modeVersion
          package = PackageId {name = localPackageName name, version = ver}
      in StatusReleased (buildReleasedPackage package results)

    toFailedPackage name TargetFailed {target = Target {modeVersion}, failure} =
      StatusFailed FailedPackage {
        package = PackageId {
          name = localPackageName name,
          version = modeReleaseVersion sv modeVersion
        },
        reason = errorMessage failure
      }

    toSkipped name _ = StatusSkipped (SkippedPackage {name})

releaseStatuses ::
  ∀ mode .
  ReleaseState mode ('InProgress 'Uploading) ->
  [PackageStatus]
releaseStatuses ReleaseState {version = sv, targets, failed, excluded} =
  releaseStatusesFrom sv targets failed excluded

-- | Build release output from decomposed status list.
releaseOutputFrom ::
  Maybe Version ->
  Maybe BranchName ->
  [PackageStatus] ->
  ReleaseOutput
releaseOutputFrom sharedVer branch allStatuses =
  ReleaseOutput {
    success = not (any isFailure allStatuses),
    title = releaseTitle allStatuses sharedVer,
    message = releaseMessage allStatuses,
    branch
  }

releaseOutput ::
  ∀ mode .
  Maybe Version ->
  Maybe BranchName ->
  ReleaseState mode ('InProgress 'Uploading) ->
  ReleaseOutput
releaseOutput sharedVer branch uploading =
  releaseOutputFrom sharedVer branch (releaseStatuses uploading)

writeReleaseOutput :: OutputTarget -> OutputFormat -> ReleaseOutput -> M ()
writeReleaseOutput =
  writeOutput $ const \case
    OutputSpecialCommitMsg -> unsupported "commit-msg"
    OutputSpecialGaPr -> unsupported "ga-pr"
    OutputSpecialGithubOutputKey -> pure "release"
  where
    unsupported :: ∀ a . Text -> M a
    unsupported format =
      clientError [exon|Output format #{Color.yellow format} isn't supported for #{Color.green @Text "release"}|]

-- TODO what should this do when the @ReleaseStage@ isn't @Uploading@?
-- Probably some error object that the pipeline can use for accessibility, though the actual error will be there anyway
outputResults ::
  OutputTarget ->
  OutputFormat ->
  Maybe Version ->
  Maybe BranchName ->
  ReleaseStage ->
  M ()
outputResults target format sharedVer branch ReleaseStage {tag, state = ReleaseState {..}} =
  case tag of
    SInProgress SUploading ->
      writeReleaseOutput target format (releaseOutputFrom sharedVer branch (releaseStatusesFrom version targets failed excluded))
    _ ->
      unit
