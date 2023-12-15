module Hix.Managed.Data.CabalTarget where

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

data CabalTarget =
  CabalTarget {
    dep :: PackageSpecifier UnresolvedSourcePackage,
    pref :: Maybe PackagePreference
  }

-- TODO better if this used TargetBound
-- even better if it was abstracted out
cabalTarget :: ManagedOp -> PackageName -> PackageParams -> CabalTarget
cabalTarget op package params =
  CabalTarget {dep, pref}
  where
    dep = NamedPackage cabalName [PackagePropertyVersion range]

    range = packageParamsRange op params

    pref | params.oldest = Just (PackageInstalledPreference cabalName PreferOldest)
         | otherwise = Nothing

    cabalName = PackageName.toCabal package

cabalTargets :: ManagedOp -> SolverParams -> [CabalTarget]
cabalTargets op params =
  ntTo params (cabalTarget op)

candidateTarget :: PackageId -> CabalTarget
candidateTarget PackageId {name, version} =
  CabalTarget {dep = NamedPackage cabalName [PackagePropertyVersion (thisVersion version)], pref = Nothing}
  where
    cabalName = PackageName.toCabal name
