module Hix.Deps where

import Control.Monad.Extra (mapMaybeM)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Set as Set
import Distribution.Version (orLaterVersion, version0)
import Exon (exon)

import Hix.Class.Map (NtMap, convert, convert1, ntAmend1, ntElems1, ntFromList, ntKeysSet, ntMap, via, (!!))
import Hix.Data.Bounds (Bounds, TargetBounds)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep, withVersion)
import Hix.Data.Deps (ProjectDep (LocalDep, RemoteDep), TargetDeps, TargetRemoteDeps, overDep, projectDepPackage)
import Hix.Data.PackageName (LocalPackage, isLocalPackage)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackages (ManagedPackages))
import Hix.Managed.Data.Targets (Targets, getTargets)
import Hix.Monad (M, noteClient)

-- | Validate the targets, requiring that they refer to entries in the dependencies read from the flake config.
--
-- Remove all targets from the dependencies, since we don't want to find new bounds for them.
-- Keep non-target local dependencies since we need their transitives in the solver.
depsFromConfig :: ManagedPackages -> [LocalPackage] -> M TargetDeps
depsFromConfig (ManagedPackages packages) targets = do
  ps <- mapMaybeM pkgDeps targets
  pure (ntFromList ps)
  where
    pkgDeps pkg
      | notTarget pkg =
        pure Nothing

      | otherwise = do
        package <- noteClient (notFound pkg) (packages !? pkg)
        pure (Just (pkg, ntFromList (targetDep <$> package.deps)))

    targetDep dep
      | isLocalPackage locals dep.package
      = (dep.package, LocalDep dep)
      | otherwise
      = (dep.package, RemoteDep dep)

    notTarget pkg = not (Set.member pkg targetsSet)

    targetsSet = Set.fromList targets

    locals = ntKeysSet packages

    notFound :: LocalPackage -> Text
    notFound pkg = [exon|PackageId '##{pkg}' not found in the flake config|]

withManagedRanges ::
  TargetBounds ->
  TargetDeps ->
  TargetDeps
withManagedRanges =
  ntAmend1 (overDep . withVersion)

forTargets ::
  NtMap map LocalPackage v sort =>
  Targets ->
  map ->
  map
forTargets targets =
  via (flip Map.restrictKeys (Set.fromList (getTargets targets)))

depsToTargetBounds :: TargetRemoteDeps -> TargetBounds
depsToTargetBounds = convert1 (.version)

mergeBounds :: TargetRemoteDeps -> Bounds
mergeBounds =
  convert (.version) . mconcat . Map.elems . ntMap

distributeBounds :: Bounds -> TargetRemoteDeps -> TargetBounds
distributeBounds bounds =
  convert1 \ dep -> fromMaybe (orLaterVersion version0) (bounds !! dep.package)

uniqueDeps :: TargetDeps -> [ProjectDep]
uniqueDeps =
  nubOrdOn projectDepPackage . ntElems1

uniqueRemoteDeps :: TargetRemoteDeps -> [Dep]
uniqueRemoteDeps =
  nubOrdOn (.package) . ntElems1
