module Hix.Managed.Cabal.Solve where

import Distribution.Client.Dependency (
  DepResolverParams,
  PackagePreference,
  PackageSpecifier,
  addPreferences,
  foldProgress,
  resolveDependencies,
  setAllowBootLibInstalls,
  standardInstallPolicy,
  )
import Distribution.Client.Dependency.Types (Solver (Modular))
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.Types (UnresolvedSourcePackage)
import Distribution.Simple.Utils (debugNoWrap)
import Distribution.Solver.Types.Settings (AllowBootLibInstalls (AllowBootLibInstalls))
import Distribution.Verbosity (Verbosity)
import Exon (exon)

import Hix.Data.Error (Error (Fatal))
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Changes (SolverPlan, solverPlan)
import qualified Hix.Managed.Cabal.Data.Config
import qualified Hix.Managed.Cabal.Data.SolveResources
import Hix.Managed.Cabal.Data.SolveResources (SolveResources)
import qualified Hix.Managed.Cabal.Data.SolveTarget
import Hix.Managed.Cabal.Data.SolveTarget (SolveTarget)
import qualified Hix.Managed.Cabal.Data.SolverState
import Hix.Managed.Cabal.Data.SolverState (SolverState (SolverState), compileSolverFlags)
import Hix.Managed.Cabal.Targets (solveTargets)
import Hix.Monad (M, tryIOMAs)

newtype Unresolvable =
  Unresolvable Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

logMsg :: Verbosity -> String -> IO a -> IO a
logMsg verbosity message rest =
  debugNoWrap verbosity message *> rest

solveSpecifiers ::
  SolveResources ->
  (DepResolverParams -> DepResolverParams) ->
  [PackageSpecifier UnresolvedSourcePackage] ->
  [PackagePreference] ->
  IO (Either String SolverInstallPlan)
solveSpecifiers res mapParams pkgSpecifiers prefs =
  foldProgress (logMsg res.conf.verbosity) (pure . Left) (pure . Right) $
  resolveDependencies res.platform res.compiler res.pkgConfigDb Modular params
  where
    params =
      mapParams $
      res.solverParams $
      setAllowBootLibInstalls (AllowBootLibInstalls res.conf.allowBoot) $
      addPreferences prefs $
      standardInstallPolicy res.installedPkgIndex res.sourcePkgDb pkgSpecifiers

solveForTargets ::
  SolveResources ->
  (DepResolverParams -> DepResolverParams) ->
  [SolveTarget] ->
  M (Either Unresolvable SolverInstallPlan)
solveForTargets res mapParams targets =
  first fromString <$> tryIOMAs (Fatal "Cabal solver crashed.") (solveSpecifiers res mapParams pkgSpecifiers prefs)
  where
    pkgSpecifiers = (.dep) <$> targets
    prefs = (.prefs) =<< targets

solveWithCabal' ::
  SolveResources ->
  SolverState ->
  M (Maybe SolverInstallPlan)
solveWithCabal' solveResources SolverState {constraints, flags} = do
  solveForTargets solveResources (compileSolverFlags flags) targets >>= \case
    Right plan ->
      pure (Just plan)
    Left err -> do
      Log.debug [exon|Solver found no plan: ##{err}|]
      pure Nothing
  where
    targets = solveTargets constraints

solveWithCabal ::
  SolveResources ->
  SolverState ->
  M (Maybe SolverPlan)
solveWithCabal solveResources solverState =
  fmap solverPlan <$> solveWithCabal' solveResources solverState
