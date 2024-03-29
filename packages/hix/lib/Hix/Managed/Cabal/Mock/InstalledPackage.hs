module Hix.Managed.Cabal.Mock.InstalledPackage where

import Distribution.InstalledPackageInfo (InstalledPackageInfo (..), emptyInstalledPackageInfo)
import Distribution.Package (UnitId, mkUnitId)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)

import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Managed.Cabal.Data.Packages (InstalledPackages (InstalledPackages))
import Hix.Pretty (showP)

mockUnitId :: PackageId -> UnitId
mockUnitId = mkUnitId . showP

installedPackageInfo :: PackageId -> [UnitId] -> InstalledPackageInfo
installedPackageInfo package depends =
  emptyInstalledPackageInfo {
    sourcePackageId = PackageId.toCabal package,
    installedUnitId = mockUnitId package,
    depends
  }

mockInstalledPackageIndex :: InstalledPackages -> InstalledPackageIndex
mockInstalledPackageIndex =
  PackageIndex.fromList .
  fmap (uncurry installedPackageInfo . second (fmap mockUnitId)) .
  coerce
