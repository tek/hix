module Hix.Managed.Release.ReleasePlan where

import Data.List.Extra (nubOrd)
import qualified Data.Set as Set
import Exon (exon)

import Hix.Class.Map (nDelete, nFromList, nInsert, nMap, nNull, nTo, (!?))
import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.Version (Version)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..), ReleaseVersion (..))
import Hix.Managed.Data.ReleaseContext (ReleaseContext (..), ReleasePackage (..))
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..))
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion, explicitVersion, implicitVersion, keepVersion)
import Hix.Managed.Release.Data.TargetSpec (TargetSpec (..), targetSpecName, targetSpecVersion)
import Hix.Managed.Report (plural)
import Hix.Managed.VersionIncrement (incrementVersion)
import Hix.Monad (appContext, clientError)
import Hix.Pretty (showPL)

versionNotGreater :: Version -> Version -> Text
versionNotGreater current new =
  [exon|Specified version #{Color.green new} isn't greater than current version #{Color.green current}|]

-- | Result of restricting release targets to the specified packages.
-- Contains selected packages, excluded packages, and explicit version overrides.
data RestrictedTargets =
  RestrictedTargets {
    selected :: Packages ReleasePackage,
    excluded :: Packages ReleasePackage,
    explicitVersions :: Packages Version
  }

restrictTargets ::
  NonEmpty TargetSpec ->
  Packages ReleasePackage ->
  M RestrictedTargets
restrictTargets specified packages =
  handleError partitioned
  where
    handleError (selected, excluded, explicit, unknown) = case unknown of
      Just pkgs -> clientError (err pkgs)
      Nothing -> pure RestrictedTargets {selected, excluded, explicitVersions = nFromList explicit}

    partitioned = foldr' step ([], packages, [], Nothing) specified

    step target (selected, available, explicit, unknown)
      | Just mp <- available !? name =
          let newExplicit = maybe explicit (\v -> (name, v) : explicit) (targetSpecVersion target)
          in (nInsert name mp selected, nDelete name available, newExplicit, unknown)
      | otherwise = (selected, available, explicit, Just (Set.insert name (fromMaybe [] unknown)))
      where
        name = LocalPackage (targetSpecName target)

    err unknown = [exon|Unknown package#{plural (length unknown)}: #{showPL unknown}|]

updateVersion :: Version -> ReleaseVersion -> SelectedVersion
updateVersion current = \case
  ConcreteVersion new -> explicitVersion new
  KeepVersion -> keepVersion current
  VersionIncrement inc -> explicitVersion (incrementVersion current inc)

currentSharedVersion :: Packages ReleasePackage -> Maybe Version
currentSharedVersion packages =
  case nubOrd (nTo packages \ _ pkg -> pkg.version) of
    [version] -> Just version
    _ -> Nothing

-- | Compute the configured version for a target package.
-- Priority: explicit version from --package > shared version from --version > increment from --version > keep current
configuredTarget ::
  ReleaseConfig ->
  Packages Version ->
  Maybe SelectedVersion ->
  ReleasePackage ->
  ConfiguredTarget
configuredTarget config explicitVersions sharedVersion ReleasePackage {name, version = current} =
  ConfiguredTarget {current, version, selected = True}
  where
    version =
      (explicitVersion <$> explicitVersions !? name)
      <|>
      sharedVersion
      <|>
      (updateVersion current <$> config.version)

excludedTarget :: ReleasePackage -> ConfiguredTarget
excludedTarget ReleasePackage {version} =
  ConfiguredTarget {current = version, version = Nothing, selected = False}

-- | Determine the shared version for a release, guarded on there being no explicit per-package versions.
-- When @--version@ is given, applies it to the shared current version.
-- When @--version@ is absent, falls back to an implicit shared version (the current version) so the UI
-- can default to shared mode.
-- Returns @Nothing@ when per-package explicit versions exist, since there is no single shared version.
createSharedVersion :: ReleaseConfig -> Packages Version -> Packages ReleasePackage -> Maybe SelectedVersion
createSharedVersion config explicitVersions selected =
  if nNull explicitVersions
  then explicit <|> implicit
  else Nothing
  where
    implicit = implicitVersion <$> currentSharedVersion selected
    explicit = updateVersion <$> currentSharedVersion selected <*> config.version

configuredReleaseVersions ::
  ReleaseContext ->
  ReleaseConfig ->
  M (Maybe SelectedVersion, Packages ConfiguredTarget)
configuredReleaseVersions ReleaseContext {packages} config =
  appContext "determining release versions from config" do
    RestrictedTargets {selected, excluded, explicitVersions} <- maybe allTargets restrictTargets config.targets packages
    let sharedVersion = createSharedVersion config explicitVersions selected
        targets = nMap (configuredTarget config explicitVersions sharedVersion) selected
    pure (sharedVersion, nMap excludedTarget excluded <> targets)
  where
    allTargets pkgs = pure RestrictedTargets {selected = pkgs, excluded = [], explicitVersions = []}
