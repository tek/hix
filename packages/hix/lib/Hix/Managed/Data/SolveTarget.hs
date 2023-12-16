module Hix.Managed.Data.SolveTarget where

import Distribution.Client.Dependency (
  PackagePreference (PackageInstalledPreference),
  PackageProperty (PackagePropertyVersion),
  PackageSpecifier (NamedPackage),
  )
import Distribution.Client.Types (UnresolvedSourcePackage)
import Distribution.Solver.Types.InstalledPreference (InstalledPreference (PreferOldest))
import Distribution.Version (thisVersion)

import Hix.Class.Map (ntTo)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import qualified Hix.Data.PackageName as PackageName
import Hix.Data.PackageName (PackageName)
import Hix.Managed.Data.ManagedConfig (ManagedOp)
import qualified Hix.Managed.Data.SolverParams
import Hix.Managed.Data.SolverParams (PackageParams, SolverParams, packageParamsRange)

data SolveTarget =
  SolveTarget {
    dep :: PackageSpecifier UnresolvedSourcePackage,
    pref :: Maybe PackagePreference
  }

-- TODO better if this used TargetBound
-- even better if it was abstracted out
solveTarget :: ManagedOp -> PackageName -> PackageParams -> SolveTarget
solveTarget op package params =
  SolveTarget {dep, pref}
  where
    dep = NamedPackage cabalName [PackagePropertyVersion range]

    range = packageParamsRange op params

    pref | params.oldest = Just (PackageInstalledPreference cabalName PreferOldest)
         | otherwise = Nothing

    cabalName = PackageName.toCabal package

solveTargets :: ManagedOp -> SolverParams -> [SolveTarget]
solveTargets op params =
  ntTo params (solveTarget op)

candidateTarget :: PackageId -> SolveTarget
candidateTarget PackageId {name, version} =
  SolveTarget {dep = NamedPackage cabalName [PackagePropertyVersion (thisVersion version)], pref = Nothing}
  where
    cabalName = PackageName.toCabal name
