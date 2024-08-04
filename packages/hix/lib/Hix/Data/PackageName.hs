module Hix.Data.PackageName where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Set as Set
import qualified Distribution.Package as Cabal
import Distribution.Package (depPkgName, mkPackageName)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.Dependency (Dependency)

import Hix.Class.EncodeNix (EncodeNixKey)
import Hix.Pretty (prettyText)

newtype PackageName =
  PackageName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, ToString, Ord, FromJSON, FromJSONKey, ToJSON, ToJSONKey, EncodeNixKey)

instance Pretty PackageName where
  pretty (PackageName n) = prettyText n

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
  LocalPackage { name :: PackageName }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, ToString, Ord, FromJSON, FromJSONKey, ToJSON, ToJSONKey, Pretty, EncodeNixKey)

localPackageName :: LocalPackage -> PackageName
localPackageName = coerce

localPackageNames :: Functor f => f LocalPackage -> f PackageName
localPackageNames = fmap coerce

sameLocalPackage :: LocalPackage -> PackageName -> Bool
sameLocalPackage (LocalPackage lp) p = lp == p

isLocalPackage :: Set LocalPackage -> PackageName -> Bool
isLocalPackage lps p = Set.member (LocalPackage p) lps

toLocalPackage :: Set LocalPackage -> PackageName -> Maybe LocalPackage
toLocalPackage lps p
  | isLocalPackage lps p
  = Just (LocalPackage p)
  | otherwise
  = Nothing
