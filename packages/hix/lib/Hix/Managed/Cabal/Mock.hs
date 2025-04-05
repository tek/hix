{-# language CPP #-}

module Hix.Managed.Cabal.Mock where

import Distribution.Simple (buildCompilerId, unknownCompilerInfo)
import Distribution.Simple.Compiler (AbiTag (NoAbiTag))
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..))
import Distribution.System (Arch (X86_64), OS (Linux), Platform (Platform))

import Hix.Data.PackageName (LocalPackage)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import qualified Hix.Managed.Cabal.Data.SolveResources
import Hix.Managed.Cabal.Data.SolveResources (SolveResources (SolveResources))
import Hix.Managed.Cabal.Init (emptySolveFlags)
import Hix.Managed.Cabal.Mock.InstalledPackage (mockInstalledPackageIndex)
import qualified Hix.Managed.Cabal.Mock.SourcePackage as SourcePackage
import Hix.Managed.Cabal.Mock.SourcePackage (mockSourcePackageDb)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)

mockSolveResources ::
  Packages ManagedPackage ->
  GhcPackages ->
  (SolveResources, Set LocalPackage)
mockSolveResources packages GhcPackages {installed, available} =
  (resources, localUnavailable)
  where
    resources =
      SolveResources {
        conf = def,
        flags = emptySolveFlags,
        platform = Platform X86_64 Linux,
        compiler = unknownCompilerInfo buildCompilerId NoAbiTag,
#if MIN_VERSION_cabal_install_solver(3,14,0)
        pkgConfigDb = PkgConfigDb mempty,
#else
        pkgConfigDb = NoPkgConfigDb,
#endif
        installedPkgIndex = mockInstalledPackageIndex installed,
        sourcePkgDb,
        solverParams = id
      }

    (sourcePkgDb, localUnavailable) = SourcePackage.dbWithManaged packages (mockSourcePackageDb available)
