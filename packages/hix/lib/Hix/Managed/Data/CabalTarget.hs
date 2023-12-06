module Hix.Managed.Data.CabalTarget where

import Distribution.Client.Dependency (
  PackagePreference (PackageInstalledPreference),
  PackageProperty (PackagePropertyVersion),
  PackageSpecifier (NamedPackage),
  )
import Distribution.Client.Types (UnresolvedSourcePackage)
import Distribution.Solver.Types.InstalledPreference (InstalledPreference (PreferOldest))
import Distribution.Version (anyVersion, intersectVersionRanges, orEarlierVersion, orLaterVersion, thisVersion)

import Hix.Class.Map (ntTo)
import Hix.Data.Package (PackageName, packageNameToCabal)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpBump, OpLowerInit, OpLowerOptimize, OpLowerStabilize))
import qualified Hix.Managed.Data.SolverParams
import Hix.Managed.Data.SolverParams (BoundMutation (..), PackageParams (PackageParams), SolverParams)

data CabalTarget =
  CabalTarget {
    dep :: PackageSpecifier UnresolvedSourcePackage,
    pref :: Maybe PackagePreference
  }

-- TODO better if this used TargetBound
-- even better if it was abstracted out
cabalTarget :: ManagedOp -> PackageName -> PackageParams -> CabalTarget
cabalTarget op package PackageParams {oldest, mutation, bounds} =
  CabalTarget {dep, pref}
  where
    dep = NamedPackage cabalName [PackagePropertyVersion range]

    range = intersectVersionRanges (mutationRange mutation) userRange

    mutationRange = \case
      ExtendedBound v -> extended op v
      RetractedBound v -> retracted op v
      NoBounds -> anyVersion

    userRange | Just r <- bounds = r
              | otherwise = anyVersion

    extended = \case
      OpBump -> orLaterVersion
      OpLowerStabilize -> orEarlierVersion
      OpLowerInit -> orEarlierVersion
      OpLowerOptimize -> orEarlierVersion

    retracted = \case
      OpBump -> orEarlierVersion
      OpLowerStabilize -> orLaterVersion
      OpLowerInit -> orLaterVersion
      OpLowerOptimize -> orLaterVersion

    pref | oldest = Just (PackageInstalledPreference cabalName PreferOldest)
         | otherwise = Nothing

    cabalName = packageNameToCabal package

cabalTargets :: ManagedOp -> SolverParams -> [CabalTarget]
cabalTargets op params =
  ntTo params (cabalTarget op)

candidateTarget :: NewVersion -> CabalTarget
candidateTarget NewVersion {package, version} =
  CabalTarget {dep = NamedPackage cabalName [PackagePropertyVersion (thisVersion version)], pref = Nothing}
  where
    cabalName = packageNameToCabal package
