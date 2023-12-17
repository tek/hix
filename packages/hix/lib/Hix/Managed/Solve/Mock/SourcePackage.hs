module Hix.Managed.Solve.Mock.SourcePackage where

import qualified Data.Map.Strict as Map
import Distribution.Client.Types (PackageLocation (LocalTarballPackage), SourcePackageDb (..), UnresolvedSourcePackage)
import Distribution.PackageDescription (
  CondTree (..),
  Library (libBuildInfo),
  LibraryVisibility (LibraryVisibilityPublic),
  PackageDescription (..),
  emptyLibrary,
  emptyPackageDescription,
  )
import Distribution.Pretty (Pretty (pretty))
import Distribution.Simple (Version)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..), emptyGenericPackageDescription)
import Distribution.Types.Library (libVisibility)
import Exon (exon)
import GHC.Exts (IsList)

import Hix.Class.Map (
  LookupMaybe,
  LookupMonoid,
  NtMap,
  convert,
  convertWithKey,
  ntKeys,
  ntMap,
  ntPretty1,
  ntPrettyWith,
  ntTo1,
  via,
  (!!),
  )
import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (LocalPackage (LocalPackage), PackageName)
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..), ManagedPackages)
import Hix.Monad (noteClient)
import Hix.Pretty (prettyL)

newtype SourcePackageDeps =
  SourcePackageDeps (Map Version [Dep])
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NtMap SourcePackageDeps Version [Dep] LookupMonoid where

instance Pretty SourcePackageDeps where
  pretty = ntPrettyWith prettyL

allDeps :: [Dep] -> SourcePackageDeps -> SourcePackageDeps
allDeps deps = via (fmap (deps ++))

allDep :: Dep -> SourcePackageDeps -> SourcePackageDeps
allDep dep = via (fmap (dep :))

newtype SourcePackages =
  SourcePackages (Map PackageName SourcePackageDeps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NtMap SourcePackages PackageName SourcePackageDeps LookupMaybe where

instance Pretty SourcePackages where
  pretty = ntPretty1

newtype SourcePackageVersions =
  SourcePackageVersions (Map PackageName [Version])
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NtMap SourcePackageVersions PackageName [Version] LookupMaybe where

instance Pretty SourcePackageVersions where
  pretty = ntPrettyWith prettyL

sourcePackageVersions :: SourcePackages -> SourcePackageVersions
sourcePackageVersions = convert (sort . ntKeys)

queryVersions :: SourcePackageVersions -> PackageName -> M [Version]
queryVersions packages query = noteClient [exon|No such package in the source db: ##{query}|] (packages !! query)

queryPackages :: SourcePackages -> PackageName -> M [Version]
queryPackages packages =
  queryVersions versions
  where
    versions = sourcePackageVersions packages

queryVersionsLatest :: SourcePackageVersions -> PackageName -> M (Maybe Version)
queryVersionsLatest packages query = last <$> queryVersions packages query

queryPackagesLatest :: SourcePackages -> PackageName -> M (Maybe Version)
queryPackagesLatest packages =
  queryVersionsLatest versions
  where
    versions = sourcePackageVersions packages

mockSourcePackage :: PackageName -> Version -> [Dep] -> UnresolvedSourcePackage
mockSourcePackage name version deps =
  SourcePackage {
    srcpkgPackageId = cabalId,
    srcpkgDescription = emptyGenericPackageDescription {
      packageDescription = emptyPackageDescription {
        package = cabalId
      },
      condLibrary = Just library
    },
    srcpkgSource = LocalTarballPackage "/invalid",
    srcpkgDescrOverride = Nothing
  }
  where
    library =
      mempty {
        condTreeData = emptyLibrary {
          libVisibility = LibraryVisibilityPublic,
          libBuildInfo = mempty
        },
        condTreeConstraints = Dep.toCabal <$> deps
      }
    cabalId = PackageId.toCabal PackageId {..}

mockSourcePackageDb :: SourcePackages -> SourcePackageDb
mockSourcePackageDb packages =
  SourcePackageDb {
    packageIndex = PackageIndex.fromList (ntTo1 packages mockSourcePackage),
    packagePreferences = []
  }

managedSourcePackageVersions :: ManagedPackages -> SourcePackageVersions
managedSourcePackageVersions =
  convertWithKey \ (LocalPackage name) package -> (name, [package.version])

managedSourcePackage :: ManagedPackage -> UnresolvedSourcePackage
managedSourcePackage ManagedPackage {name = LocalPackage name, version, deps} =
  mockSourcePackage name version deps

managedPackageIndex :: ManagedPackages -> PackageIndex UnresolvedSourcePackage
managedPackageIndex packages =
  PackageIndex.fromList (managedSourcePackage <$> Map.elems (ntMap packages))

dbWithManaged :: ManagedPackages -> SourcePackageDb -> SourcePackageDb
dbWithManaged packages SourcePackageDb {packageIndex, packagePreferences} =
  SourcePackageDb {
    packageIndex = PackageIndex.merge packageIndex (managedPackageIndex packages),
    packagePreferences
  }
