module Hix.Managed.ManagedPackageProto where

import Hix.Class.Map (nFromList, nKeysSet, nMap)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage (ManagedPackage))
import qualified Hix.Managed.Data.ManagedPackageProto
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto (ManagedPackageProto))
import Hix.Managed.Data.Mutable (classifyPackageDep)
import Hix.Managed.Data.Packages (Packages)

validate ::
  Packages ManagedPackageProto ->
  Packages ManagedPackage
validate proto =
  nMap package proto
  where
    package ManagedPackageProto {..} =
      ManagedPackage {
        package = name,
        version,
        local = nFromList localDeps,
        mutable = nFromList mutableDeps
      }
      where
        (localDeps, mutableDeps) = partitionEithers (classifyPackageDep locals <$> deps)

    locals = nKeysSet proto
