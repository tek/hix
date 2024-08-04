module Hix.Data.PackageId where

import Distribution.Package (PackageIdentifier (PackageIdentifier))
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version)
import Exon (exon)

import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName (..))
import Hix.Pretty (prettyText, showP)
import Distribution.Parsec ( Parsec(..) )

data PackageId =
  PackageId {
    name :: PackageName,
    version :: Version
  }
  deriving stock (Eq, Show, Ord, Generic)

renderPackage :: PackageId -> Text
renderPackage PackageId {..} =
  [exon|##{name}-#{showP version}|]

instance Pretty PackageId where
  pretty = prettyText . renderPackage

toCabal :: PackageId -> PackageIdentifier
toCabal PackageId {..} =
  PackageIdentifier (PackageName.toCabal name) version

fromCabal :: PackageIdentifier -> PackageId
fromCabal (PackageIdentifier (PackageName.fromCabal -> name) version) =
  PackageId {..}

instance Parsec PackageId where
  parsec = fromCabal <$> parsec
