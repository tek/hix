{-# language CPP #-}

module Hix.Managed.Cabal.Resources where

import Distribution.Client.IndexUtils (getInstalledPackages, getSourcePackages)
import qualified Distribution.Client.NixStyleOptions
import Distribution.Client.Setup (configCompilerAux', withRepoContext)
import Distribution.Simple (pattern GlobalPackageDB, compilerInfo)
import Distribution.Solver.Types.PkgConfigDb (readPkgConfigDb)
import Distribution.Verbosity (Verbosity, lessVerbose, silent, verbose)

import qualified Hix.Data.Monad
import Hix.Data.Monad (M, appRes)
import Hix.Data.PackageName (LocalPackage)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Config (
  CabalConfig (..),
  GhcDb (GhcDbSynthetic, GhcDbSystem),
  SolveConfig (..),
  allHackages,
  )
import qualified Hix.Managed.Cabal.Data.SolveResources
import Hix.Managed.Cabal.Data.SolveResources (SolveResources (SolveResources))
import qualified Hix.Managed.Cabal.Init
import Hix.Managed.Cabal.Init (initialize)
import Hix.Managed.Cabal.Mock (mockSolveResources)
import qualified Hix.Managed.Cabal.Mock.SourcePackage as SourcePackage
import Hix.Managed.Data.ManagedPackage (ProjectPackages (..))
import Hix.Monad (tryIOM)

#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Simple (PackageDBStackCWD)
#else
import Distribution.Simple (PackageDBStack)
#endif

#if MIN_VERSION_cabal_install_solver(3,14,0)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..))
#endif

#if MIN_VERSION_Cabal(3,14,0)
packageDbs :: PackageDBStackCWD
#else
packageDbs :: PackageDBStack
#endif
packageDbs = [GlobalPackageDB]

-- | This adds 'ManagedPackage's to the source package DB (the set of available package IDs) if there is no available
-- candidate in Cabal's snapshots for a given local package.
-- This may happen when the package hasn't been published to Hackage yet.
--
-- This means that the solver will find our local packages (for targets that depend on packages in other envs) like it
-- finds Hackage packages, and therefore non-target local deps will be included in the plan.
--
-- Because we don't want local packages in the plan (as they are not mutable, but static in the Nix build), it would be
-- tempting to add 'ManagedPackage's to the installed package index instead, which would exclude them from the plan's
-- overrides.
-- However, the metadata of installed packages must include concrete unit IDs for their dependencies with fixed
-- versions, which would require us to choose versions for them and might interfere with solving.
solveResources ::
  ProjectPackages ->
  SolveConfig ->
  M (SolveResources, Set LocalPackage)
solveResources packages conf = do
  flags <- initialize conf
  Log.debug "Acquiring Cabal resources."
  tryIOM $ withRepoContext conf.verbosity flags.global \ repoContext -> do
    (compiler, platform, progdb) <- configCompilerAux' flags.main.configFlags
    pkgConfigDb <- readPkgConfigDb conf.verbosity progdb
    installedPkgIndex <- getInstalledPackages conf.verbosity compiler packageDbs progdb
    remoteSourcePkgDb <- getSourcePackages conf.verbosity repoContext
    let
      (sourcePkgDb, localUnavailable) = SourcePackage.dbWithManaged (coerce packages) remoteSourcePkgDb
      resources = SolveResources {
        compiler = compilerInfo compiler,
        installedPkgIndex = installedPkgIndex,
        solverParams = id,
#if MIN_VERSION_cabal_install_solver(3,14,0)
        pkgConfigDb = fromMaybe (PkgConfigDb []) pkgConfigDb,
#endif
        ..
      }
    pure (resources, localUnavailable)

cabalVerbosity :: M Verbosity
cabalVerbosity =
  appRes.cabalVerbose <&> \case
    True -> verbose
    False -> lessVerbose silent

-- TODO The Packages ManagedPackage are added in 'resources' as well as in 'mockSolveResources', which is probably ok
-- since they now come from @processProject@ and not from the tests, I think. still would be better to unify those
--
-- just add the managed packages to the result.
acquire ::
  ProjectPackages ->
  CabalConfig ->
  GhcDb ->
  M (SolveResources, Set LocalPackage)
acquire packages cabal = \case
  GhcDbSystem ghc -> do
    verbosity <- cabalVerbosity
    solveResources packages SolveConfig {hackageRepos = allHackages cabal, verbosity, ghc, allowBoot = False, cabal}
  GhcDbSynthetic db ->
    pure (mockSolveResources (coerce packages) db)
