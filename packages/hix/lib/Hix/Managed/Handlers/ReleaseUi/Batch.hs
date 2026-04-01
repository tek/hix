module Hix.Managed.Handlers.ReleaseUi.Batch where

import Hix.Class.Map (nFilter, nKeysSet, nMapWithKey, nTraverseWithKey)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage (..))
import Hix.Managed.Data.Packages (Packages (..))
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import Hix.Managed.Handlers.ReleaseUi (ReleaseUi (..))
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseTarget (..))
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..), implicitVersion)
import Hix.Managed.Release.Data.Staged (PreparedTargetView, SelectedTargetView)
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow (..))
import Hix.Managed.Release.Data.VersionChoice (SharedTarget (..), VersionChoice (..))
import Hix.Managed.Release.Validation (ProblematicVersion)

-- | Select versions for packages in batch mode.
-- If a shared version is provided, uses 'SharedVersion' variant.
-- Otherwise, uses 'IndividualVersions' with per-package versions.
chooseVersions ::
  Maybe SelectedVersion ->
  Packages ConfiguredTarget ->
  M (Either TerminateFlow VersionChoice)
chooseVersions sharedVersion targets =
  case sharedVersion of
    Just sv -> do
      let selected = nMapWithKey (toSharedTarget sv.version) selectedTargets
      pure (Right SharedVersion {version = sv, sharedTargets = selected, sharedExcluded = excludedTargets})
    Nothing -> do
      individual <- nTraverseWithKey toIndividualTarget selectedTargets
      pure (Right IndividualVersions {individualTargets = individual, individualExcluded = excludedTargets})
  where
    selectedTargets = nFilter (.selected) targets
    excludedTargets = nFilter (not . (.selected)) targets

    toSharedTarget :: Version -> LocalPackage -> ConfiguredTarget -> SharedTarget
    toSharedTarget _ name ConfiguredTarget {current} =
      SharedTarget {package = name, current}

    toIndividualTarget name ConfiguredTarget {current, version = versionSpec} = do
      let version = fromMaybe (implicitVersion current) versionSpec
      pure ReleaseTarget {current, version, package = name}

-- | Select packages for upload. In batch mode with staged types,
-- all 'PreparedTargetView's are ready for upload (failures happen after selection).
-- When @partial@ is 'False', we select all packages (failures will be handled after upload).
chooseUploadTargets ::
  ReleaseConfig ->
  UploadStage ->
  Packages PreparedTargetView ->
  M (Either TerminateFlow (Set LocalPackage))
chooseUploadTargets _config _ packages =
  pure (Right (nKeysSet packages))

-- | Non-interactive dist targets handler: if checks passed, continue with all packages.
-- If checks failed, return 'Left' to terminate (do not allow force in batch mode).
chooseDistTargets ::
  Bool ->
  Packages SelectedTargetView ->
  M (Either TerminateFlow (Set LocalPackage))
chooseDistTargets checksPassed packages
  | checksPassed = pure (Right (nKeysSet packages))
  | otherwise = pure (Left (TerminateFlow "Flake checks failed. Use --interactive to force release despite failures."))

-- | Non-interactive version problems handler: always return 'Left' to terminate.
-- In batch mode, problematic versions should fail.
chooseVersionProblems ::
  Packages ProblematicVersion ->
  M (Either TerminateFlow Bool)
chooseVersionProblems _ =
  pure (Left (TerminateFlow "Version validation failed. Use --force-version to bypass validation or --interactive to confirm."))

handlersBatch ::
  ReleaseConfig ->
  ReleaseUi
handlersBatch config =
  ReleaseUi {
    chooseVersions,
    chooseUploadTargets = chooseUploadTargets config,
    chooseDistTargets = chooseDistTargets,
    chooseVersionProblems
  }
