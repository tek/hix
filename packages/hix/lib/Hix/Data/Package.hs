module Hix.Data.Package where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON)
import qualified Distribution.Package as Cabal
import Distribution.Package (depPkgName, mkPackageName)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.Dependency (Dependency)
import Distribution.Version (Version)
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (text)

import Hix.Class.EncodeNix (EncodeNixKey)
import Hix.Pretty (showP)

newtype PackageName =
  PackageName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, ToJSON, EncodeNixKey)

instance Pretty PackageName where
  pretty (PackageName n) = text (toString n)

packageNameFromCabal :: Cabal.PackageName -> PackageName
packageNameFromCabal =
  fromString . Cabal.unPackageName

packageNameToCabal :: PackageName -> Cabal.PackageName
packageNameToCabal (PackageName name) =
  mkPackageName (toString name)

depPackageName :: Dependency -> PackageName
depPackageName =
  packageNameFromCabal . depPkgName

newtype LocalPackage =
  LocalPackage PackageName
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, Pretty, EncodeNixKey)

localPackageName :: LocalPackage -> PackageName
localPackageName = coerce

localPackageNames :: [LocalPackage] -> [PackageName]
localPackageNames = coerce

data Package =
  Package {
    name :: PackageName,
    version :: Version
  }
  deriving stock (Eq, Show, Generic)

renderPackage :: Package -> Text
renderPackage Package {..} =
  [exon|##{name}-#{showP version}|]

instance Pretty Package where
  pretty = PrettyPrint.text . toString . renderPackage
