module Hix.Managed.ManagedPackage where

import Control.Lens (each, (%~))
import Distribution.Version (VersionRange, removeLowerBound)

import Hix.Class.Map (nFlatten, nGenWith, nKeys, nMap, nOver)
import Hix.Data.Bounds (Ranges)
import Hix.Data.Dep (Dep (..))
import Hix.Data.PackageName (LocalPackage, PackageName)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage, ProjectPackages)
import qualified Hix.Managed.Data.Mutable as Mutable
import Hix.Managed.Data.Mutable (MutableRanges)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.Targets (Targets, sortTargets)

forTargets ::
  ProjectPackages ->
  [LocalPackage] ->
  (Targets, MutableRanges)
forTargets packages targetNames =
  (targets, nFlatten id targetRanges)
  where
    targetRanges :: Packages MutableRanges
    targetRanges = Mutable.forTargets targets allRanges

    targets = sortTargets onlyDepNames targetNames

    onlyDepNames :: Packages [PackageName]
    onlyDepNames = nMap nKeys allRanges

    allRanges :: Packages Ranges
    allRanges =
      nOver onlyDeps $ nGenWith \ Dep {package, version} -> (package, version)

    onlyDeps :: Packages [Dep]
    onlyDeps = nMap (.deps) packages

updateRanges ::
  (PackageName -> VersionRange -> VersionRange) ->
  ManagedPackage ->
  ManagedPackage
updateRanges f =
  #deps . each %~ \ Dep {version = original, ..} ->
    Dep {version = f package original, ..}

overRanges :: (VersionRange -> VersionRange) -> ManagedPackage -> ManagedPackage
overRanges f = #deps . each . #version %~ f

removeLowerBounds :: ManagedPackage -> ManagedPackage
removeLowerBounds = overRanges removeLowerBound
