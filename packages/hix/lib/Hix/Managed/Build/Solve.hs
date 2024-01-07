module Hix.Managed.Build.Solve where

import qualified Data.Set as Set
import Distribution.Pretty (pretty)
import Exon (exon)
import Text.PrettyPrint (hang)

import Hix.Class.Map (nRestrictKeys)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (isLocalPackage)
import Hix.Data.Version (packageIdVersions)
import qualified Hix.Log as Log
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Changes (SolverChanges (SolverChanges), SolverPlan (SolverPlan))
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvDeps)
import Hix.Managed.Data.Mutable (depName)
import qualified Hix.Managed.Handlers.Cabal
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import Hix.Pretty (showPL)

logNonReinstallable :: NonEmpty PackageId -> M ()
logNonReinstallable ids =
  Log.verbose [exon|NOTE: Cabal solver suggested new versions for non-reinstallable packages: #{showPL ids}|]

-- TODO Do we need to filter locals out of @matching@ or can those not be in there?
processSolverPlan ::
  EnvDeps ->
  SolverPlan ->
  M SolverChanges
processSolverPlan deps SolverPlan {..} = do
  Log.debugP (hang "New project deps from solver:" 2 (pretty projectDeps))
  traverse_ logNonReinstallable nonReinstallable
  pure SolverChanges {versions, overrides, projectDeps}
  where
    projectDeps = nRestrictKeys mutablePIds versions
    versions = packageIdVersions (overrides ++ matching)
    overrides = filter notLocal changes
    notLocal PackageId {name} = not (isLocalPackage deps.local name)
    mutablePIds = Set.fromList (depName <$> Set.toList deps.mutable)

solveMutation ::
  CabalHandlers ->
  EnvDeps ->
  SolverState ->
  M (Maybe SolverChanges)
solveMutation cabal deps state =
  traverse (processSolverPlan deps) =<< cabal.solveForVersion state
