module Hix.Data.Package where

import Data.Aeson (FromJSON, FromJSONKey)
import qualified Distribution.Package as Cabal
import Distribution.Package (depPkgName)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.Dependency (Dependency)
import Text.PrettyPrint (text)

newtype PackageName =
  PackageName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey)

instance Pretty PackageName where
  pretty (PackageName n) = text (toString n)

packageNameFromCabal :: Cabal.PackageName -> PackageName
packageNameFromCabal =
  fromString . Cabal.unPackageName

depPackageName :: Dependency -> PackageName
depPackageName =
  packageNameFromCabal . depPkgName

newtype LocalPackage =
  LocalPackage PackageName
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, Pretty)

localPackageName :: LocalPackage -> PackageName
localPackageName = coerce

localPackageNames :: [LocalPackage] -> [PackageName]
localPackageNames = coerce
