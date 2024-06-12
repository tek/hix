module Hix.Managed.Cabal.Mock.SourcePackage where

import qualified Data.Map.Strict as Map
import Distribution.Client.Types (PackageLocation (LocalTarballPackage), SourcePackageDb (..), UnresolvedSourcePackage)
import Distribution.PackageDescription (
  BuildInfo (targetBuildDepends),
  CondTree (..),
  Library (libBuildInfo),
  LibraryVisibility (LibraryVisibilityPublic),
  PackageDescription (..),
  emptyLibrary,
  emptyPackageDescription,
  )
import Distribution.Simple (Version)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import qualified Distribution.Solver.Types.SourcePackage as Cabal
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..), emptyGenericPackageDescription)
import Distribution.Types.Library (libVisibility)
import Exon (exon)

import Hix.Class.Map (nGet, nKeys, nMap, nTo1, nTransform, (!!))
import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (LocalPackage (LocalPackage), PackageName, localPackageName)
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackage, SourcePackageId (..), SourcePackageVersions, SourcePackages)
import qualified Hix.Managed.Data.ManagedPackage as ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Monad (noteClient)

allDeps :: [Dep] -> SourcePackage -> SourcePackage
allDeps deps =
  nMap \ p -> p {deps = deps ++ p.deps}
  where

allDep :: Dep -> SourcePackage -> SourcePackage
allDep = allDeps . pure

sourcePackageVersions :: SourcePackages -> SourcePackageVersions
sourcePackageVersions = nMap (sort . nKeys)

queryVersions :: SourcePackageVersions -> PackageName -> M [Version]
queryVersions packages query = noteClient [exon|No such package in the source db: ##{query}|] (packages !! query)

queryPackages :: SourcePackages -> PackageName -> M [Version]
queryPackages packages =
  queryVersions (sourcePackageVersions packages)

queryVersionsLatest :: SourcePackageVersions -> PackageName -> M (Maybe Version)
queryVersionsLatest packages query = last <$> queryVersions packages query

queryPackagesLatest :: SourcePackages -> PackageName -> M (Maybe Version)
queryPackagesLatest packages =
  queryVersionsLatest versions
  where
    versions = sourcePackageVersions packages

-- | This _must_ have the dependencies in both 'targetBuildDepends' and 'condTreeConstraints', otherwise they will be
-- ignored or deleted depending on what resolver params are used.
-- This is also evidenced by looking at a parsed Cabal file, see @CabalTest@.
mockSourcePackage :: PackageName -> Version -> SourcePackageId -> UnresolvedSourcePackage
mockSourcePackage name version SourcePackageId {description, deps} =
  Cabal.SourcePackage {
    srcpkgPackageId = cabalId,
    srcpkgDescription = emptyGenericPackageDescription {
      packageDescription = (fromMaybe emptyPackageDescription description) {
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
          libBuildInfo = mempty {targetBuildDepends = cabalDeps}
        },
        condTreeConstraints = cabalDeps
      }
    cabalId = PackageId.toCabal PackageId {..}
    cabalDeps = Dep.toCabal <$> deps

mockSourcePackageDb :: SourcePackages -> SourcePackageDb
mockSourcePackageDb packages =
  SourcePackageDb {
    packageIndex = PackageIndex.fromList (nTo1 packages mockSourcePackage),
    packagePreferences = []
  }

managedSourcePackageVersions :: Packages ManagedPackage -> SourcePackageVersions
managedSourcePackageVersions =
  nTransform \ (LocalPackage name) package -> (name, [package.version])

managedSourcePackage :: ManagedPackage -> UnresolvedSourcePackage
managedSourcePackage package =
  mockSourcePackage (localPackageName package.package) package.version pid
  where
    pid = SourcePackageId {deps = ManagedPackage.deps package, description = Nothing}

managedPackageIndex :: Packages ManagedPackage -> PackageIndex UnresolvedSourcePackage
managedPackageIndex packages =
  PackageIndex.fromList (managedSourcePackage <$> Map.elems (nGet packages))

dbWithManaged :: Packages ManagedPackage -> SourcePackageDb -> SourcePackageDb
dbWithManaged packages SourcePackageDb {packageIndex, packagePreferences} =
  SourcePackageDb {
    packageIndex = PackageIndex.merge packageIndex (managedPackageIndex packages),
    packagePreferences
  }
