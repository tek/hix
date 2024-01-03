module Hix.Managed.Data.ManagedPackage where

import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.Map (nTo, nTransform)
import Hix.Data.Bounds (Ranges)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep (Dep))
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (LocalPackage (LocalPackage), localPackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Data.Mutable (LocalRanges, MutableRanges, depName)

data ManagedPackage =
  ManagedPackage {
    package :: LocalPackage,
    version :: Version,
    local :: LocalRanges,
    mutable :: MutableRanges
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ManagedPackage where
  pretty ManagedPackage {..} =
    hang (pretty package) 2 (pretty local $+$ pretty mutable)

ranges :: ManagedPackage -> Ranges
ranges ManagedPackage {local, mutable} =
  nTransform (\ k v -> (localPackageName k, v)) local <>
  nTransform (\ k v -> (depName k, v)) mutable

packageId :: ManagedPackage -> PackageId
packageId ManagedPackage {package = LocalPackage name, version} =
  PackageId {..}

deps :: ManagedPackage -> [Dep]
deps mp =
  nTo (ranges mp) \ package version -> Dep {..}
