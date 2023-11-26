module Hix.Managed.Solve where

import Data.List.Extra (nubOrdOn)
import qualified Data.Text as Text
import Distribution.Client.Dependency (
  PackageSpecifier,
  foldProgress,
  resolveDependencies,
  setAllowBootLibInstalls,
  standardInstallPolicy,
  )
import Distribution.Client.Dependency.Types (Solver (Modular))
import Distribution.Client.GlobalFlags (RepoContext, withRepoContext)
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.Targets (readUserTargets, resolveUserTargets)
import Distribution.Client.Types (UnresolvedSourcePackage, packageIndex)
import Distribution.Simple.Utils (debugNoWrap)
import Distribution.Solver.Types.Settings (AllowBootLibInstalls (AllowBootLibInstalls))
import Distribution.Verbosity (Verbosity)
import Exon (exon)

import Hix.Class.Map (ntList)
import Hix.Data.Bounds (Bounds)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep, mainDep, newVersionDep, renderDep)
import Hix.Data.Version (NewVersion)
import Hix.Error (Error (Fatal))
import qualified Hix.Log as Log
import Hix.Managed.Solve.Changes (SolverPlan, solverPlan)
import qualified Hix.Managed.Solve.Config
import qualified Hix.Managed.Solve.Init
import qualified Hix.Managed.Solve.Resources
import Hix.Managed.Solve.Resources (SolveResources (SolveResources))
import Hix.Monad (M, tryIOM, tryIOMAs)
import Hix.Pretty (showP)

newtype Unresolvable =
  Unresolvable Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

logMsg :: Verbosity -> String -> IO a -> IO a
logMsg verbosity message rest =
  debugNoWrap verbosity message *> rest

specifiers ::
  SolveResources ->
  [Dep] ->
  RepoContext ->
  IO [PackageSpecifier UnresolvedSourcePackage]
specifiers res targets repoContext = do
  userTargets <- readUserTargets res.conf.verbosity targetSpecs
  resolveUserTargets res.conf.verbosity repoContext (packageIndex res.sourcePkgDb) userTargets
  where
    targetSpecs = toString . renderDep <$> targets

solveSpecifiers ::
  SolveResources ->
  [PackageSpecifier UnresolvedSourcePackage] ->
  IO (Either String SolverInstallPlan)
solveSpecifiers SolveResources {..} pkgSpecifiers =
  foldProgress (logMsg conf.verbosity) (pure . Left) (pure . Right) $
  resolveDependencies platform compiler pkgConfigDb Modular params
  where
    params =
      solverParams $
      setAllowBootLibInstalls (AllowBootLibInstalls conf.allowBoot) $
      standardInstallPolicy installedPkgIndex sourcePkgDb pkgSpecifiers

solveTargets ::
  SolveResources ->
  [Dep] ->
  M (Either Unresolvable SolverInstallPlan)
solveTargets res targets = do
  pkgSpecifiers <- tryIOM (withRepoContext res.conf.verbosity res.flags.global (specifiers res uniqueTargets))
  Log.debug [exon|Running solver for: #{Text.intercalate ", " (renderDep <$> uniqueTargets)}|]
  first fromString <$> tryIOMAs (Fatal "Cabal solver crashed.") (solveSpecifiers res pkgSpecifiers)
  where
    uniqueTargets = nubOrdOn (.package) targets

solveWithCabal ::
  SolveResources ->
  Bounds ->
  NewVersion ->
  M (Maybe SolverPlan)
solveWithCabal solveResources solverBounds newVersion =
  solveTargets solveResources solveDeps >>= \case
    Right plan -> do
      Log.debug [exon|Solver found a plan for #{showP newVersion}|]
      pure (Just (solverPlan plan))
    Left err -> do
      Log.debug [exon|Solver found no plan for #{showP newVersion}: ##{err}|]
      pure Nothing
  where
    solveDeps = newVersionDep newVersion : (uncurry mainDep <$> ntList solverBounds)
