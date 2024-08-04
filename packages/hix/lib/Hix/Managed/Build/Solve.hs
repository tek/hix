module Hix.Managed.Build.Solve where

import Data.List (partition)
import qualified Data.Set as Set
import Distribution.PackageDescription (customFieldsPD)
import Distribution.Pretty (pretty)
import Exon (exon)
import Text.PrettyPrint (hang, ($$), (<+>))

import Hix.Class.Map (nRestrictKeys)
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId (PackageId))
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
import Hix.Pretty (prettyL, showPL)

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
  Set PackageId ->
  SolverPlan ->
  M SolverChanges
processSolverPlan forceRevisions cabal deps prevRevisions SolverPlan {..} = do
  Log.debugP $
    hang "New project deps from solver:" 2 (pretty projectDeps) $$
    "Forced revisions:" <+> prettyL forcedRevisions $$
    "Reused revisions:" <+> prettyL reusedRevisions
  traverse_ logNonReinstallable nonReinstallable
  pure SolverChanges {versions, overrides}
  where
    projectDeps = nRestrictKeys mutablePIds versions
    versions = packageIdVersions (overrides ++ installed)
    overrides = filter notLocal (changes ++ forcedRevisions ++ reusedRevisions)
    (reusedRevisions, installed) = partition (flip Set.member prevRevisions) noForcedRevisions
    (noForcedRevisions, forcedRevisions) = partitionEithers (checkRevision forceRevisions cabal <$> matching)
    -- notLocal PackageId {name} = not (isLocalPackage deps.local name)
    -- TODO I assumed that targets hadn't been part of EnvDeps.local for a long time, so this shouldn't be effective
    -- anymore, but verify anyway!
    notLocal PackageId {} = True
    mutablePIds = Set.fromList (depName <$> Set.toList deps.mutable)

-- TODO probably best to store the revisions in the SolverState
solveMutation ::
  CabalHandlers ->
  EnvDeps ->
  Set PackageId ->
  SolverState ->
  M (Maybe SolverChanges)
solveMutation cabal deps prevRevisions state =
  traverse (processSolverPlan state.flags.forceRevisions cabal deps prevRevisions) =<< cabal.solveForVersion state
