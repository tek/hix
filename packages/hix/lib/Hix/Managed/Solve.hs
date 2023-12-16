module Hix.Managed.Solve where

import qualified Data.Map.Strict as Map
import Distribution.Client.Dependency (
  PackagePreference,
  PackageSpecifier,
  addPreferences,
  foldProgress,
  resolveDependencies,
  setAllowBootLibInstalls,
  standardInstallPolicy,
  )
import Distribution.Client.Dependency.Types (Solver (Modular))
import Distribution.Client.GlobalFlags (RepoContext)
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.Targets (readUserTargets, resolveUserTargets)
import Distribution.Client.Types (UnresolvedSourcePackage, packageIndex)
import Distribution.Simple.Utils (debugNoWrap)
import Distribution.Solver.Types.Settings (AllowBootLibInstalls (AllowBootLibInstalls))
import Distribution.Verbosity (Verbosity)
import Exon (exon)

import Hix.Class.Map (via)
import Hix.Data.Dep (Dep, renderDep)
import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId)
import qualified Hix.Log as Log
import Hix.Managed.Data.ManagedConfig (ManagedOp)
import qualified Hix.Managed.Data.SolveTarget
import Hix.Managed.Data.SolveTarget (SolveTarget, candidateTarget, solveTargets)
import Hix.Managed.Data.SolverParams (SolverParams)
import Hix.Managed.Solve.Changes (SolverPlan, solverPlan)
import qualified Hix.Managed.Solve.Config
import qualified Hix.Managed.Solve.Resources
import Hix.Managed.Solve.Resources (SolveResources)
import Hix.Monad (M, tryIOMAs)
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
  [PackagePreference] ->
  IO (Either String SolverInstallPlan)
solveSpecifiers res pkgSpecifiers prefs =
  foldProgress (logMsg res.conf.verbosity) (pure . Left) (pure . Right) $
  resolveDependencies res.platform res.compiler res.pkgConfigDb Modular params
  where
    params =
      res.solverParams $
      setAllowBootLibInstalls (AllowBootLibInstalls res.conf.allowBoot) $
      addPreferences prefs $
      standardInstallPolicy res.installedPkgIndex res.sourcePkgDb pkgSpecifiers

solveForTargets ::
  SolveResources ->
  [SolveTarget] ->
  M (Either Unresolvable SolverInstallPlan)
solveForTargets res targets =
  first fromString <$> tryIOMAs (Fatal "Cabal solver crashed.") (solveSpecifiers res pkgSpecifiers prefs)
  where
    pkgSpecifiers = (.dep) <$> targets
    prefs = mapMaybe (.pref) targets

solveWithCabal ::
  SolveResources ->
  ManagedOp ->
  SolverParams ->
  PackageId ->
  M (Maybe SolverPlan)
solveWithCabal solveResources op solverParams newVersion =
  solveForTargets solveResources targets >>= \case
    Right plan -> do
      Log.debug [exon|Solver found a plan for #{showP newVersion}|]
      pure (Just (solverPlan plan))
    Left err -> do
      Log.debug [exon|Solver found no plan for #{showP newVersion}: ##{err}|]
      pure Nothing
  where
    targets = candidateTarget newVersion : solveTargets op (via (Map.delete newVersion.name) solverParams)
