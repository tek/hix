module Hix.Managed.Solve.Resources where

import Control.Monad.Trans.Reader (asks)
import Distribution.Client.Dependency (DepResolverParams)
import Distribution.Client.IndexUtils (getSourcePackages)
import qualified Distribution.Client.NixStyleOptions
import Distribution.Client.Setup (configCompilerAux', withRepoContext)
import Distribution.Client.Types (SourcePackageDb)
import Distribution.Simple (CompilerInfo, PackageDB (GlobalPackageDB), PackageDBStack, compilerInfo)
import Distribution.Simple.GHC (getInstalledPackages)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb, readPkgConfigDb)
import Distribution.System (Platform)
import Distribution.Verbosity (silent, verbose)
import Path (Abs, Dir, Path)

import qualified Hix.Log as Log
import Hix.Managed.Solve.Config (SolveConfig (..))
import qualified Hix.Managed.Solve.Init
import Hix.Managed.Solve.Init (SolveFlags, initialize)
import qualified Hix.Monad
import Hix.Monad (M, tryIOM)

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

packageDbs :: PackageDBStack
packageDbs = [GlobalPackageDB]

resources ::
  SolveConfig ->
  M SolveResources
resources conf = do
  flags <- initialize conf
  Log.debug "Acquiring Cabal resources."
  tryIOM $ withRepoContext conf.verbosity flags.global \ repoContext -> do
    (compiler, platform, progdb) <- configCompilerAux' flags.main.configFlags
    pkgConfigDb <- readPkgConfigDb conf.verbosity progdb
    installedPkgIndex <- getInstalledPackages conf.verbosity compiler packageDbs progdb
    sourcePkgDb <- getSourcePackages conf.verbosity repoContext
    pure SolveResources {compiler = compilerInfo compiler, solverParams = id, ..}

acquire ::
  Maybe (Path Abs Dir) ->
  M SolveResources
acquire ghc = do
  verbosity <- asks (.debug) <&> \case
    True -> verbose
    False -> silent
  resources def {verbosity, ghc}
