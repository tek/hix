module Hix.Data.PackageId where

import Distribution.Package (PackageIdentifier (PackageIdentifier))
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version)
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint

import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName (..))
import Hix.Pretty (showP)

data PackageId =
  PackageId {
    name :: PackageName,
    version :: Version
  }
  deriving stock (Eq, Show, Generic)

renderPackage :: PackageId -> Text
renderPackage PackageId {..} =
  [exon|##{name}-#{showP version}|]

instance Pretty PackageId where
  pretty = PrettyPrint.text . toString . renderPackage

toCabal :: PackageId -> PackageIdentifier
toCabal PackageId {..} =
  PackageIdentifier (PackageName.toCabal name) version

fromCabal :: PackageIdentifier -> PackageId
fromCabal (PackageIdentifier (PackageName.fromCabal -> name) version) =
  PackageId {..}
