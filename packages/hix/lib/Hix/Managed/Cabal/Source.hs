module Hix.Managed.Cabal.Source where

import Distribution.Client.Types (SourcePackageDb (SourcePackageDb, packageIndex))
import Distribution.PackageDescription (
  GenericPackageDescription (GenericPackageDescription, packageDescription),
  PackageDescription,
  )
import Distribution.Solver.Types.PackageIndex (lookupPackageId)
import Distribution.Solver.Types.SourcePackage (SourcePackage (SourcePackage, srcpkgDescription))

import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId)

sourcePackage ::
  SourcePackageDb ->
  PackageId ->
  Maybe PackageDescription
sourcePackage SourcePackageDb {packageIndex} package =
  lookupPackageId packageIndex (PackageId.toCabal package) <&> \case
    SourcePackage {srcpkgDescription = GenericPackageDescription {packageDescription}} -> packageDescription
