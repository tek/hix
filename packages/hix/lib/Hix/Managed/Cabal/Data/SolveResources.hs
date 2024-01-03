module Hix.Managed.Cabal.Data.SolveResources where

import Distribution.Client.Dependency (DepResolverParams)
import Distribution.Client.Types (SourcePackageDb)
import Distribution.Simple (CompilerInfo)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.System (Platform)

import Hix.Managed.Cabal.Data.Config (SolveConfig (..))
import Hix.Managed.Cabal.Init (SolveFlags)

data SolveResources =
  SolveResources {
    conf :: SolveConfig,
    flags :: SolveFlags,
    platform :: Platform,
    compiler :: CompilerInfo,
    pkgConfigDb :: PkgConfigDb,
    installedPkgIndex :: InstalledPackageIndex,
    sourcePkgDb :: SourcePackageDb,
    solverParams :: DepResolverParams -> DepResolverParams
  }
