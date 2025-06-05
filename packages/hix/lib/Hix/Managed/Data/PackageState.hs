module Hix.Managed.Data.PackageState where

import Data.Aeson (FromJSON (..), withObject, (.:?))
import Distribution.Pretty (Pretty (pretty))

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Data.Json (jsonParsec)
import Hix.Data.Version (Version)

data PackageState =
  PackageState {
    -- | The version of the latest release.
    -- If the package hasn't been released, or the release process is currently in progress, this may be the version of
    -- the upcoming release.
    version :: Maybe Version
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (EncodeNix)

instance FromJSON PackageState where
  parseJSON =
    withObject "PackageState" \ o -> do
      version <- fmap jsonParsec <$> o .:? "version"
      pure PackageState {..}

instance Pretty PackageState where
  pretty PackageState {..} = maybe "no version" pretty version
