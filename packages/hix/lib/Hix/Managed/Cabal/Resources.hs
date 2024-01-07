module Hix.Managed.Cabal.Resources where

import Control.Monad.Trans.Reader (asks)
import Distribution.Client.IndexUtils (getInstalledPackages, getSourcePackages)
import qualified Distribution.Client.NixStyleOptions
import Distribution.Client.Setup (configCompilerAux', withRepoContext)
import Distribution.Simple (PackageDB (GlobalPackageDB), PackageDBStack, compilerInfo)
import Distribution.Solver.Types.PkgConfigDb (readPkgConfigDb)
import Distribution.Verbosity (lessVerbose, silent, verbose)

import qualified Hix.Data.Monad
import Hix.Data.Monad (M (M))
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSynthetic, GhcDbSystem), SolveConfig (..))
import qualified Hix.Managed.Cabal.Data.SolveResources
import Hix.Managed.Cabal.Data.SolveResources (SolveResources (SolveResources))
import qualified Hix.Managed.Cabal.Init
import Hix.Managed.Cabal.Init (initialize)
import Hix.Managed.Cabal.Mock (mockSolveResources)
import qualified Hix.Managed.Cabal.Mock.SourcePackage as SourcePackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Monad (tryIOM)

packageDbs :: PackageDBStack
packageDbs = [GlobalPackageDB]

-- | This adds the 'ManagedPackage's to the source package DB, which is the set of available package IDs.
-- This means that the solver will find our local packages (for targets that depend on packages in other envs) like it
-- finds Hackage packages, and therefore local deps will be included in the plan.
--
-- Because we don't want local packages in the plan (as they are not mutable, but static in the Nix build), it would be
-- tempting to add 'ManagedPackage's to the installed package index instead, which would exclude them from the plan's
-- overrides.
-- However, their metadata must include concrete unit IDs for their dependencies with fixed versions, which would
-- require us to choose versions for them and might interfere with solving.
--
-- Maybe the basic installed package index could be queried to determine the dep versions.
-- Not sure this would be better than just filtering the plan.
resources ::
  Packages ManagedPackage ->
  SolveConfig ->
  M SolveResources
resources packages conf = do
  flags <- initialize conf
  Log.debug "Acquiring Cabal resources."
  tryIOM $ withRepoContext conf.verbosity flags.global \ repoContext -> do
    (compiler, platform, progdb) <- configCompilerAux' flags.main.configFlags
    pkgConfigDb <- readPkgConfigDb conf.verbosity progdb
    installedPkgIndex <- getInstalledPackages conf.verbosity compiler packageDbs progdb
    sourcePkgDb <- getSourcePackages conf.verbosity repoContext
    pure SolveResources {
      compiler = compilerInfo compiler,
      installedPkgIndex = installedPkgIndex,
      sourcePkgDb = SourcePackage.dbWithManaged packages sourcePkgDb,
      solverParams = id,
      ..
    }

-- TODO The Packages ManagedPackage are added in 'resources' as well as in 'mockSolveResources', which is probably ok since they
-- now come from @processProject@ and not from the tests, I think. still would be better to unify those
--
-- just add the managed packages to the result.
acquire ::
  Packages ManagedPackage ->
  GhcDb ->
  M SolveResources
acquire packages = \case
  GhcDbSystem ghc -> do
    verbosity <- M (asks (.debug)) <&> \case
      True -> verbose
      False -> lessVerbose silent
    resources packages def {verbosity, ghc}
  GhcDbSynthetic db ->
    pure (mockSolveResources packages db)
