module Hix.Managed.Release.Data.SelectedVersion where

import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (text, (<+>))

import Hix.Data.Version (Version)

-- | Information about a version selection for a release.
-- Tracks the user's intention to determine validation behavior.
data SelectedVersion =
  SelectedVersion {
    -- | The version to release.
    version :: Version,
    -- | Whether the user explicitly specified this version via CLI or UI.
    -- If 'False', the version was computed automatically.
    explicit :: Bool,
    -- | Whether the user explicitly requested keeping the current version.
    -- Set when @--version keep@ was specified.
    keep :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Pretty SelectedVersion where
  pretty SelectedVersion {..} =
    pretty version <+> flags
    where
      flags
        | keep = text "(keep)"
        | explicit = text "(explicit)"
        | otherwise = mempty

-- | Create a selected version that was explicitly specified by the user.
explicitVersion :: Version -> SelectedVersion
explicitVersion version =
  SelectedVersion {version, explicit = True, keep = False}

-- | Create a selected version for keeping the current version.
keepVersion :: Version -> SelectedVersion
keepVersion version =
  SelectedVersion {version, explicit = True, keep = True}

-- | Create a selected version that was computed automatically.
implicitVersion :: Version -> SelectedVersion
implicitVersion version =
  SelectedVersion {version, explicit = False, keep = False}
