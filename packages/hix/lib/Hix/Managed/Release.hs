module Hix.Managed.Release where

import Exon (exon)
import Path (Abs, File, Path)

import Hix.Class.Map (nMap, nNull, nSize)
import qualified Hix.Data.Monad as AppResources
import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.Options (ReleaseOptions (..))
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Config (cabalConfig)
import qualified Hix.Managed.Cabal.Data.UploadStage as UploadStage
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import Hix.Managed.Data.ReleaseContext (ReleaseContext (..), ReleaseContextProto (..))
import Hix.Managed.Data.UploadResult (ArtifactResult)
import Hix.Managed.Git (BranchName, runGitApi)
import Hix.Managed.Git.Native (BracketResult (..))
import Hix.Managed.Handlers.Context (ContextKey (ContextRelease), jsonOrQueryProd)
import Hix.Managed.Handlers.Release (ReleaseHandlers (..))
import qualified Hix.Managed.Handlers.Release.Prod as Release
import Hix.Managed.Handlers.ReleaseUi (ReleaseUi (..))
import Hix.Managed.Release.Compat (cliCompat)
import qualified Hix.Managed.Release.Context as ReleaseContext
import Hix.Managed.Release.Data.GitResult (GitResult (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseDist)
import Hix.Managed.Release.Data.Staged (ReleaseState (..), Termination (..), UploadingTargetView (..))
import qualified Hix.Managed.Release.Flow as ReleaseFlow
import Hix.Managed.Release.Flow (ReleaseFlow, runReleaseFlow, withTargetsAndShared)
import Hix.Managed.Release.Git (CommitResult (..), CommitStyle (..), GitExtraArgs (..), GitRelease (..))
import Hix.Managed.Release.Hook (withHooks)
import Hix.Managed.Release.ReleasePlan (configuredReleaseVersions)
import Hix.Managed.Release.ReleaseResult (outputResults)
import qualified Hix.Managed.Release.Report as Report
import Hix.Managed.Release.Staged (ReleaseStage (..))
import Hix.Managed.Release.StateVersions (updateStateVersions)
import Hix.Managed.Release.Validation (ProblematicVersion, findProblematicVersions, validateVersionsBatch)
import Hix.Monad (appContext, ask, clientError, local)
import Hix.Pretty (showP)

-- | Handle post-upload activities: execute hooks and commit git changes.
-- This runs hooks for all successfully completed packages, allowing them to perform additional actions like updating
-- changelogs or release notes.
-- The commit style depends on whether a shared version was used.
postUpload ::
  ReleaseConfig ->
  [Path Abs File] ->
  GitRelease ->
  Int ->
  Maybe Version ->
  Packages UploadingTargetView ->
  M CommitResult
postUpload config hooks git totalPackages sharedVersion uploading =
  withHooks config hooks uploading sharedVersion do
    git.commit (nMap extractVersion uploading) style
  where
    extractVersion UploadingTargetView {releaseVersion} = releaseVersion

    style
      | totalPackages == 1, Just v <- sharedVersion = CommitShared v
      | nSize uploading == 1, totalPackages > 1 = CommitIndividual
      | Just v <- sharedVersion = CommitShared v
      | otherwise = CommitIndividual

-- | Validate selected versions, showing UI in interactive mode or failing in batch mode.
validateFlowVersions ::
  ReleaseConfig ->
  ReleaseUi ->
  ReleaseFlow ()
validateFlowVersions config ui =
  unless config.forceVersion do
    targets <- ReleaseFlow.chosen
    let problems = findProblematicVersions targets
    unless (nNull problems) do
      if config.interactive
      then handleProblemsInteractive ui problems
      else ReleaseFlow.lift (validateVersionsBatch problems)

-- | Handle version problems in interactive mode.
handleProblemsInteractive ::
  ReleaseUi ->
  Packages ProblematicVersion ->
  ReleaseFlow ()
handleProblemsInteractive ui problems =
  ReleaseFlow.lift (ui.chooseVersionProblems problems) >>= \case
    Left reason -> ReleaseFlow.terminate reason
    Right accepted -> unless accepted do
      ReleaseFlow.lift (validateVersionsBatch problems)

-- | Run checks if enabled by config, otherwise skip.
runChecksIfEnabled ::
  ReleaseConfig ->
  ReleaseHandlers ->
  M (Maybe [Text])
runChecksIfEnabled config handlers =
  if config.check
  then do
    Log.info "Running flake checks"
    handlers.runChecks
  else do
    Log.info "Skipping flake checks"
    pure Nothing

-- | Upload a single stage with logging.
uploadFlowStage ::
  ReleaseConfig ->
  ReleaseUi ->
  (Text -> UploadStage.UploadStage -> LocalPackage -> ReleaseDist -> M ArtifactResult) ->
  UploadStage.UploadStage ->
  ReleaseFlow ()
uploadFlowStage config ui uploadArtifact stage =
  ReleaseFlow.uploadStage config stage chooseAndLog (uploadArtifact (showP stage) stage)
  where
    chooseAndLog s targets = do
      Log.info [exon|Uploading #{showP s}|]
      ui.chooseUploadTargets s targets

-- | Orchestrate the complete release flow.
--
-- The flow progresses through stages:
--
-- 1. __Version Selection__: Determine release versions (shared or per-package)
-- 2. __Validation__: Check for problematic versions (too large jumps, same as current)
-- 3. __State Update__: Update managed state files with new versions
-- 4. __Checks__: Run flake checks if enabled
-- 5. __Distribution__: Build release artifacts (source tarballs, docs)
-- 6. __Upload__: Upload artifacts to Hackage (candidates then publish)
-- 7. __Commit__: Create git commit and tags, execute hooks
--
-- Each stage can terminate the flow early if errors occur.
-- The @partial@ config option controls whether failures affect other packages.
releaseFlow ::
  ReleaseHandlers ->
  GitRelease ->
  ReleaseContext ->
  ReleaseConfig ->
  ReleaseFlow (Maybe Version, CommitResult)
releaseFlow handlers@ReleaseHandlers {ui} git context config = do
  (shared, configured) <- ReleaseFlow.lift (configuredReleaseVersions context config)
  ReleaseFlow.initTargets (ui.chooseVersions shared) configured
  validateFlowVersions config ui
  withTargetsAndShared (updateStateVersions handlers context.managed config.forceVersion)
  checksPassed <- ReleaseFlow.checksStage (runChecksIfEnabled config handlers)
  ReleaseFlow.initDists checksPassed ui.chooseDistTargets handlers.releaseDist
  ReleaseFlow.initUploading
  traverse_ @[] (uploadFlowStage config ui handlers.uploadArtifact) [minBound .. maxBound]
  commitResult <- ReleaseFlow.withUploading (postUpload config context.hooks git (nSize context.packages))
  sharedVer <- ReleaseFlow.getSharedVersion
  pure (sharedVer, commitResult)

release ::
  ReleaseHandlers ->
  ReleaseConfig ->
  ReleaseContextProto ->
  M ReleaseStage
release handlers config proto =
  appContext "releasing packages" do
    root <- appRes.root
    context <- ReleaseContext.validate proto
    let extraArgs = GitExtraArgs {commitArgs = context.commitExtraArgs, tagArgs = context.tagExtraArgs}
    (bracketResult, releaseBranch, ((sharedVer, commitResult), stage)) <-
      runGitApi (handlers.git extraArgs) root "release" \ git@GitRelease {bracket} ->
        bracket do
          runReleaseFlow (releaseFlow handlers git context config)
    let gitResult = assembleGitResult bracketResult releaseBranch commitResult
    AppResources {output = format, target} <- ask
    Report.report gitResult stage
    outputResults target format sharedVer releaseBranch stage
    pure stage

assembleGitResult :: BracketResult -> Maybe BranchName -> CommitResult -> GitResult
assembleGitResult BracketResult {initialBranch, merged, pushed} releaseBranch CommitResult {names, tags} =
  GitResult {
    committed = (,) <$> releaseBranch <*> names,
    tagged = tags,
    initialBranch,
    merged,
    pushed
  }

releaseCli :: ReleaseOptions -> M ()
releaseCli options = do
  cliCompat options
  context <- jsonOrQueryProd ContextRelease options.context
  cabal <- cabalConfig context.hackage options.cabal
  handlers <- Release.handlersProd options cabal
  local (\ res -> res {AppResources.persistentUi = options.config.persistentUi}) do
    release handlers options.config context >>= \case
      ReleaseStage {state = ReleaseState {termination = Termination _}} -> clientError "Release failed."
      _ -> unit
