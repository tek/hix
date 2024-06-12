module Hix.Managed.Cabal.Data.SourcePackage where

import Distribution.PackageDescription (PackageDescription)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Simple (Version)
import GHC.Exts (IsList)

import Hix.Class.Map (LookupMaybe, LookupMonoid, NMap, nPretty, nPretty1, nPrettyWith)
import Hix.Data.Dep (Dep)
import Hix.Data.PackageName (PackageName)
import Hix.Pretty (prettyL)

data SourcePackageId =
  SourcePackageId {
    deps :: [Dep],
    description :: Maybe PackageDescription
  }
  deriving stock (Eq, Show, Generic)

instance Pretty SourcePackageId where
  pretty SourcePackageId {deps} = prettyL deps

newtype SourcePackage =
  SourcePackage (Map Version SourcePackageId)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap SourcePackage Version SourcePackageId LookupMonoid where

instance Pretty SourcePackage where
  pretty = nPretty

newtype SourcePackages =
  SourcePackages (Map PackageName SourcePackage)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap SourcePackages PackageName SourcePackage LookupMaybe where

instance Pretty SourcePackages where
  pretty = nPretty1

newtype SourcePackageVersions =
  SourcePackageVersions (Map PackageName [Version])
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap SourcePackageVersions PackageName [Version] LookupMaybe where

instance Pretty SourcePackageVersions where
  pretty = nPrettyWith prettyL
