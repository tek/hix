module Hix.Managed.Release.Data.ReleaseTarget where

import Distribution.Pretty (Pretty (pretty))
import Path (Abs, File, Path)
import Text.PrettyPrint ((<+>))

import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage (..), localPackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))
import Hix.Pretty (field, prettyFieldsV)

data ReleaseTarget =
  ReleaseTarget {
    package :: LocalPackage,
    current :: Version,
    version :: SelectedVersion
  }
  deriving stock (Eq, Show)

instance Pretty ReleaseTarget where
  pretty ReleaseTarget {..} =
    pretty pid <+> "->" <+> pretty version.version
    where
      pid = PackageId {name = localPackageName package, version = current}

data ReleaseDist =
  ReleaseDist {
    sources :: Path Abs File,
    docs :: Path Abs File
  }
  deriving stock (Eq, Show)

instance Pretty ReleaseDist where
  pretty ReleaseDist {..} =
    prettyFieldsV [
      field "sources" sources,
      field "docs" docs
    ]
