module Hix.Managed.Solve.Mock where

import Distribution.Simple (buildCompilerId, unknownCompilerInfo)
import Distribution.Simple.Compiler (AbiTag (NoAbiTag))
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (NoPkgConfigDb))
import Distribution.System (Arch (X86_64), OS (Linux), Platform (Platform))

import Hix.Data.PackageId (PackageId)
import Hix.Managed.Solve.Init (emptySolveFlags)
import Hix.Managed.Solve.Mock.InstalledPackage (mockInstalledPackageIndex)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages, mockSourcePackageDb)
import qualified Hix.Managed.Solve.Resources
import Hix.Managed.Solve.Resources (SolveResources (SolveResources))

mockSolveResources ::
  [(PackageId, [PackageId])] ->
  SourcePackages ->
  SolveResources
mockSolveResources installed available = do
  SolveResources {
    conf = def,
    flags = emptySolveFlags,
    platform = Platform X86_64 Linux,
    compiler = unknownCompilerInfo buildCompilerId NoAbiTag,
    pkgConfigDb = NoPkgConfigDb,
    installedPkgIndex = mockInstalledPackageIndex installed,
    sourcePkgDb = mockSourcePackageDb available,
    solverParams = id
  }
