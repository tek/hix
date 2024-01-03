module Hix.Managed.Cabal.Data.SourcePackage where

import Distribution.Pretty (Pretty (pretty))
import Distribution.Simple (Version)
import GHC.Exts (IsList)

import Hix.Class.Map (LookupMaybe, LookupMonoid, NMap, nPretty1, nPrettyWith)
import Hix.Data.Dep (Dep)
import Hix.Data.PackageName (PackageName)
import Hix.Pretty (prettyL)

newtype SourcePackageDeps =
  SourcePackageDeps (Map Version [Dep])
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap SourcePackageDeps Version [Dep] LookupMonoid where

instance Pretty SourcePackageDeps where
  pretty = nPrettyWith prettyL

newtype SourcePackages =
  SourcePackages (Map PackageName SourcePackageDeps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap SourcePackages PackageName SourcePackageDeps LookupMaybe where

instance Pretty SourcePackages where
  pretty = nPretty1

newtype SourcePackageVersions =
  SourcePackageVersions (Map PackageName [Version])
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap SourcePackageVersions PackageName [Version] LookupMaybe where

instance Pretty SourcePackageVersions where
  pretty = nPrettyWith prettyL
