module Hix.Managed.Build.Solve where

import qualified Data.Set as Set
import Distribution.PackageDescription (customFieldsPD)
import Distribution.Pretty (pretty)
import Exon (exon)
import Text.PrettyPrint (hang, ($$), (<+>))

import Hix.Class.Map (nRestrictKeys)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (isLocalPackage)
import Hix.Data.Version (packageIdVersions)
import qualified Hix.Log as Log
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Changes (SolverChanges (SolverChanges), SolverPlan (SolverPlan))
import qualified Hix.Managed.Cabal.Data.SolverState
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvDeps)
import Hix.Managed.Data.Mutable (depName)
import qualified Hix.Managed.Handlers.Cabal
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import Hix.Pretty (showPL, prettyL)

logNonReinstallable :: NonEmpty PackageId -> M ()
logNonReinstallable ids =
  Log.verbose [exon|NOTE: Cabal solver suggested new versions for non-reinstallable packages: #{showPL ids}|]

checkRevision ::
  Bool ->
  CabalHandlers ->
  PackageId ->
  Either PackageId PackageId
checkRevision forceRevisions cabal package
  | forceRevisions
  , Just packageDesc <- cabal.sourcePackage package
  , any isRevision packageDesc.customFieldsPD
  = Right package
  | otherwise
  = Left package
  where
    isRevision = \case
      ("x-revision", rev) | rev /= "0" -> True
      _ -> False

processSolverPlan ::
  Bool ->
  CabalHandlers ->
  EnvDeps ->
  SolverPlan ->
  M SolverChanges
processSolverPlan forceRevisions cabal deps SolverPlan {..} = do
  Log.debugP (hang "New project deps from solver:" 2 (pretty projectDeps) $$ "Revisions:" <+> prettyL revisions)
  traverse_ logNonReinstallable nonReinstallable
  pure SolverChanges {versions, overrides, projectDeps}
  where
    projectDeps = nRestrictKeys mutablePIds versions
    versions = packageIdVersions (overrides ++ installed)
    overrides = filter notLocal (changes ++ revisions)
    (installed, revisions) = partitionEithers (checkRevision forceRevisions cabal <$> matching)
    notLocal PackageId {name} = not (isLocalPackage deps.local name)
    mutablePIds = Set.fromList (depName <$> Set.toList deps.mutable)

solveMutation ::
  CabalHandlers ->
  EnvDeps ->
  SolverState ->
  M (Maybe SolverChanges)
solveMutation cabal deps state =
  traverse (processSolverPlan state.flags.forceRevisions cabal deps) =<< cabal.solveForVersion state
