module Hix.Data.PackageName where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON)
import qualified Data.Set as Set
import qualified Distribution.Package as Cabal
import Distribution.Package (depPkgName, mkPackageName)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.Dependency (Dependency)
import Text.PrettyPrint (text)

import Hix.Class.EncodeNix (EncodeNixKey)

newtype PackageName =
  PackageName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, ToJSON, EncodeNixKey)

instance Pretty PackageName where
  pretty (PackageName n) = text (toString n)

fromCabal :: Cabal.PackageName -> PackageName
fromCabal =
  fromString . Cabal.unPackageName

toCabal :: PackageName -> Cabal.PackageName
toCabal (PackageName name) =
  mkPackageName (toString name)

depPackageName :: Dependency -> PackageName
depPackageName =
  fromCabal . depPkgName

newtype LocalPackage =
  LocalPackage PackageName
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, Pretty, EncodeNixKey)

localPackageName :: LocalPackage -> PackageName
localPackageName = coerce

localPackageNames :: [LocalPackage] -> [PackageName]
localPackageNames = coerce

sameLocalPackage :: LocalPackage -> PackageName -> Bool
sameLocalPackage (LocalPackage lp) p = lp == p

isLocalPackage :: Set LocalPackage -> PackageName -> Bool
isLocalPackage lps p = Set.member (LocalPackage p) lps
