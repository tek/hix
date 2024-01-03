module Hix.Managed.Data.MutableId where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version)

import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Managed.Data.Mutable (MutableDep (MutableDep))
import Hix.Pretty (showP)

data MutableId =
  MutableId {
    name :: MutableDep,
    version :: Version
  }
  deriving stock (Eq, Show, Generic)

toPackageId :: MutableId -> PackageId
toPackageId MutableId {name = MutableDep name, version} =
  PackageId {name, version}

instance Pretty MutableId where
  pretty = pretty . toPackageId

instance ToJSON MutableId where
  toJSON MutableId {..} =
    object ["name" .= toJSON name, "version" .= toJSON (showP version :: Text)]
