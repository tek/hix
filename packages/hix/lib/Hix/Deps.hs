module Hix.Deps where

import Control.Monad.Extra (mapMaybeM)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Set as Set
import Distribution.Version (orLaterVersion, version0)
import Exon (exon)

import Hix.Class.Map (NtMap, convert, convert1, ntAmend1, ntBy, ntElems1, ntFromList, ntMap, via, (!!))
import Hix.Data.Bounds (Bounds, TargetBounds)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep, withVersion)
import Hix.Data.Deps (TargetDeps)
import Hix.Data.PackageName (LocalPackage, PackageName, localPackageNames)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackages (ManagedPackages))
import Hix.Managed.Data.Targets (Targets, getTargets)
import Hix.Monad (M, noteClient)

-- | Validate the targets, requiring that they refer to entries in the dependencies read from the flake config.
depsFromConfig :: ManagedPackages -> [LocalPackage] -> M TargetDeps
depsFromConfig (ManagedPackages packages) targets = do
  ps <- mapMaybeM pkgDeps targets
  pure (ntFromList ps)
  where
    pkgDeps pkg
      | not (Set.member pkg targetsSet) =
        pure Nothing

      | otherwise = do
        package <- noteClient (notFound pkg) (packages !? pkg)
        pure (Just (pkg, nonlocal package.deps))

    nonlocal deps = ntBy (filter notLocal deps) (.package)

    notLocal dep = not (Set.member dep.package localNames)

    localNames :: Set PackageName
    localNames = Set.fromList (localPackageNames (Map.keys packages))

    targetsSet = Set.fromList targets

    notFound :: LocalPackage -> Text
    notFound pkg = [exon|PackageId '##{pkg}' not found in the flake config|]

withManagedRanges ::
  TargetBounds ->
  TargetDeps ->
  TargetDeps
withManagedRanges =
  ntAmend1 withVersion

forTargets ::
  NtMap map LocalPackage v sort =>
  Targets ->
  map ->
  map
forTargets targets =
  via (flip Map.restrictKeys (Set.fromList (getTargets targets)))

depsToTargetBounds :: TargetDeps -> TargetBounds
depsToTargetBounds = convert1 (.version)

mergeBounds :: TargetDeps -> Bounds
mergeBounds =
  convert (.version) . mconcat . Map.elems . ntMap

distributeBounds :: Bounds -> TargetDeps -> TargetBounds
distributeBounds bounds =
  convert1 \ dep -> fromMaybe (orLaterVersion version0) (bounds !! dep.package)

uniqueDeps :: TargetDeps -> [Dep]
uniqueDeps =
  nubOrdOn (.package) . ntElems1
