module Hix.Managed.Data.ReleaseContext where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Distribution.Pretty (Pretty (..))
import Path (Abs, Dir, File, Path, Rel)

import Hix.Data.Json (jsonParsec)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.PathSpec (PathSpec)
import Hix.Data.Version (Version)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Data.Packages (Packages)
import Hix.Pretty (HPretty (hpretty), field, prettyMap)

-- | @since unreleased
data ReleasePackage =
  ReleasePackage {
    name :: LocalPackage,
    version :: Version,
    path :: Path Rel Dir
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ReleasePackage where
  pretty ReleasePackage {..} =
    pretty PackageId {name = coerce name, version}

instance FromJSON ReleasePackage where
  parseJSON =
    withObject "ManagedPackage" \ o -> do
      name <- o .: "name"
      version <- jsonParsec <$> o .: "version"
      path <- o .: "path"
      pure ReleasePackage {..}

-- | @since unreleased
data ReleaseContextProto =
  ReleaseContextProto {
    packages :: Packages ReleasePackage,
    hackage :: Map HackageName ContextHackageRepo,
    hooks :: [PathSpec File],
    commitExtraArgs :: [Text],
    tagExtraArgs :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty ReleaseContextProto where
  hpretty ReleaseContextProto {..} =
    prettyMap "maint" [
      field "packages" packages,
      field "hackage" hackage,
      field "hooks" hooks
    ]

-- | @since unreleased
data ReleaseContext =
  ReleaseContext {
    packages :: Packages ReleasePackage,
    hackage :: Map HackageName ContextHackageRepo,
    hooks :: [Path Abs File],
    commitExtraArgs :: [Text],
    tagExtraArgs :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty ReleaseContext where
  hpretty ReleaseContext {..} =
    prettyMap "maint" [
      field "packages" packages,
      field "hackage" hackage,
      field "hooks" hooks
    ]
