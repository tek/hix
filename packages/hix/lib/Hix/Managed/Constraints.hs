module Hix.Managed.Constraints where

import Distribution.Version (thisVersion)

import Hix.Class.Map (nGenWith, nMap, nTransform)
import Hix.Data.Bounds (Ranges)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version, Versions)
import Hix.Data.VersionBounds (Bound, VersionBounds, anyBounds, hasBound)
import Hix.Managed.Data.Constraints (EnvConstraints, MutationConstraints (..))
import Hix.Managed.Data.Mutable (MutableDep (MutableDep), MutableVersions)

noBounds :: MutationConstraints
noBounds = mempty {mutation = anyBounds}

forDeps :: (PackageName -> MutationConstraints) -> Set MutableDep -> EnvConstraints
forDeps f =
  nGenWith \ (MutableDep package) -> (package, f package)

fromVersions :: (Version -> VersionBounds) -> MutableVersions -> EnvConstraints
fromVersions bound =
  nTransform \ (MutableDep package) v -> (package, mempty {mutation = foldMap bound v})

preferRanges :: Ranges -> EnvConstraints
preferRanges =
  nMap \ range -> mempty {mutation = mempty, prefer = Just range}

preferVersions :: Versions -> EnvConstraints
preferVersions = preferRanges . nMap thisVersion

preferInstalled :: Set MutableDep -> EnvConstraints
preferInstalled =
  forDeps \ _ -> mempty {mutation = mempty, installed = Just True}

preferInstalledUnlessBounded :: Bound -> EnvConstraints -> EnvConstraints
preferInstalledUnlessBounded bound =
  nMap \ old -> old {installed = Just (not (hasBound bound old.mutation))}

explicitBounds :: Ranges -> EnvConstraints
explicitBounds =
  nMap \ range -> mempty {mutation = mempty, force = Just range}
