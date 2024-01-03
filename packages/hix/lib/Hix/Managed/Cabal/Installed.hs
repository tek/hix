module Hix.Managed.Cabal.Installed where

import Distribution.Simple.PackageIndex (InstalledPackageIndex, lookupPackageName)

import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)

installedVersion ::
  InstalledPackageIndex ->
  PackageName ->
  Maybe Version
installedVersion index name =
  fst <$> head (lookupPackageName index (PackageName.toCabal name))
