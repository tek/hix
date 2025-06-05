module Hix.Managed.Release.Data.VersionChoice where

import Hix.Class.Map (nMap)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..), ReleasePlan (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseTarget (..))
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))

-- | A package target without release version info, used when the release version is shared across targets.
data SharedTarget =
  SharedTarget {
    package :: LocalPackage,
    current :: Version
  }
  deriving stock (Eq, Show, Generic)

-- | The user's choice about how versions are managed.
data VersionChoice =
  -- | All packages share the same version.
  SharedVersion {
    version :: SelectedVersion,
    sharedTargets :: Packages SharedTarget,
    sharedExcluded :: Packages ConfiguredTarget
  }
  |
  -- | Each package has its own version.
  IndividualVersions {
    individualTargets :: Packages ReleaseTarget,
    individualExcluded :: Packages ConfiguredTarget
  }
  deriving stock (Eq, Show, Generic)

sharedVersion :: VersionChoice -> Maybe SelectedVersion
sharedVersion = \case
  SharedVersion {version} -> Just version
  IndividualVersions {} -> Nothing

-- | Convert 'VersionChoice' to @'Packages' 'ReleasePlan'@ for compatibility with existing code.
-- This is a lossy conversion - the shared/individual distinction is lost.
--
-- Used in tests.
toReleasePlan :: VersionChoice -> Packages ReleasePlan
toReleasePlan = \case
  SharedVersion {version, sharedTargets, sharedExcluded} ->
    nMap (toPlannedShared version) sharedTargets <> nMap NotPlanned sharedExcluded
  IndividualVersions {individualTargets, individualExcluded} ->
    nMap Planned individualTargets <> nMap NotPlanned individualExcluded
  where
    toPlannedShared :: SelectedVersion -> SharedTarget -> ReleasePlan
    toPlannedShared selectedVersion SharedTarget {package, current} =
      Planned ReleaseTarget {
        package,
        current,
        version = selectedVersion
      }
