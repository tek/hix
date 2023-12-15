module Hix.Deps where

import Data.List.Extra (nubOrdOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Set as Set
import Distribution.Version (orLaterVersion, version0)
import Exon (exon)

import Hix.Class.Map (NtMap, convert, convert1, ntAmend1, ntElems1, ntFromList, ntMap, via, (!!))
import Hix.Data.Bounds (Bounds, TargetBounds)
import Hix.Data.ConfigDeps (
  ConfigComponentDeps (ConfigComponentDeps),
  ConfigDeps,
  ConfigPackageDeps (ConfigPackageDeps),
  )
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep, withVersion)
import Hix.Data.Deps (TargetDeps)
import Hix.Data.PackageName (LocalPackage, PackageName, localPackageNames)
import Hix.Managed.Data.Targets (Targets, getTargets)
import Hix.Monad (M, noteClient)

-- | Validate the targets, requiring that they refer to entries in the dependencies read from the flake config.
depsFromConfig :: ConfigDeps -> [LocalPackage] -> M TargetDeps
depsFromConfig confDeps targets = do
  ps <- traverse pkgDeps targets
  pure (ntFromList (catMaybes ps))
  where
    pkgDeps pkg
      | not (Set.member pkg targetsSet) =
        pure Nothing

      | otherwise = do
        ConfigPackageDeps confPdeps <- noteClient (notFound pkg) (confDeps !? pkg)
        pure (Just (pkg, ntFromList (nonlocal confPdeps)))

    nonlocal confPdeps = filter notLocal (mconcat (Map.elems (coerce confPdeps))) <&> \ dep -> (dep.package, dep)

    notLocal dep = not (Set.member dep.package localNames)

    localNames :: Set PackageName
    localNames = Set.fromList (localPackageNames (Map.keys confDeps))

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

allDeps :: TargetDeps -> [Dep]
allDeps =
  nubOrdOn (.package) . ntElems1
