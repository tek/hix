module Hix.Managed.Cabal.Mock where

import Distribution.Simple (buildCompilerId, unknownCompilerInfo)
import Distribution.Simple.Compiler (AbiTag (NoAbiTag))
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (NoPkgConfigDb))
import Distribution.System (Arch (X86_64), OS (Linux), Platform (Platform))

import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages (GhcPackages))
import qualified Hix.Managed.Cabal.Data.SolveResources
import Hix.Managed.Cabal.Data.SolveResources (SolveResources (SolveResources))
import Hix.Managed.Cabal.Init (emptySolveFlags)
import Hix.Managed.Cabal.Mock.InstalledPackage (mockInstalledPackageIndex)
import qualified Hix.Managed.Cabal.Mock.SourcePackage as SourcePackage
import Hix.Managed.Cabal.Mock.SourcePackage (mockSourcePackageDb)

mockSolveResources ::
  Packages ManagedPackage ->
  GhcPackages ->
  SolveResources
mockSolveResources packages GhcPackages {installed, available} = do
  SolveResources {
    conf = def,
    flags = emptySolveFlags,
    platform = Platform X86_64 Linux,
    compiler = unknownCompilerInfo buildCompilerId NoAbiTag,
    pkgConfigDb = NoPkgConfigDb,
    installedPkgIndex = mockInstalledPackageIndex installed,
    sourcePkgDb = SourcePackage.dbWithManaged packages (mockSourcePackageDb available),
    solverParams = id
  }
