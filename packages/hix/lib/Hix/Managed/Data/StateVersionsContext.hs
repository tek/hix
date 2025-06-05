module Hix.Managed.Data.StateVersionsContext where

import Data.Aeson (FromJSON)
import Path (File)

import Hix.Data.PathSpec (PathSpec)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Pretty (HPretty (hpretty), field, prettyMap)

-- | Per-package version file configuration.
data StateVersionsPackage =
  StateVersionsPackage {
    versionFile :: Maybe (PathSpec File)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty StateVersionsPackage where
  hpretty StateVersionsPackage {..} =
    prettyMap "package" [
      field "versionFile" versionFile
    ]

-- | Narrow context type for 'updateStateVersions'.
--
-- This contains only the state needed to update package versions during release.
-- Unlike 'ProjectContextProto', it doesn't include packages, envs, or hackage config.
data StateVersionsContext =
  StateVersionsContext {
    state :: ProjectStateProto,
    -- | Global version file for shared version mode.
    versionFile :: Maybe (PathSpec File),
    -- | Per-package version file configurations.
    packages :: Packages StateVersionsPackage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty StateVersionsContext where
  hpretty StateVersionsContext {..} =
    prettyMap "state-versions" [
      field "state" state,
      field "versionFile" versionFile,
      field "packages" packages
    ]
