module Hix.Managed.ManagedPackage where

import Hix.Class.Map (nFlatten, nKeys, nMap, nRestrictKeys, nWithoutKeys)
import Hix.Data.PackageName (LocalPackage)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Mutable (LocalRanges, MutableRanges)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.Targets (Targets, sortTargets, targetsSet)

forTargets :: Packages ManagedPackage -> [LocalPackage] -> (Targets, LocalRanges, MutableRanges)
forTargets packages targetNames =
  (targets, local, mutable)
  where
    mutable = nFlatten id (nMap (.mutable) targetPackages :: Packages MutableRanges)

    -- Remove the targets from the local deps
    local = nWithoutKeys tset (nFlatten id (nMap (.local) targetPackages :: Packages LocalRanges))

    targetPackages = nRestrictKeys tset packages

    tset = targetsSet targets

    targets = sortTargets (nMap nKeys allLocal) targetNames

    allLocal :: Packages LocalRanges
    allLocal = nMap (.local) packages
