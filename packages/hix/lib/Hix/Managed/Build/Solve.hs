module Hix.Managed.Build.Solve where

import Data.List (partition)
import qualified Data.Set as Set
import Distribution.PackageDescription (customFieldsPD)
import Distribution.Pretty (pretty)
import Exon (exon)
import Text.PrettyPrint (hang, ($$), (<+>))

import Hix.Class.Map (nRestrictKeys)
import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId)
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
import Hix.Monad (appContextVerbose)
import Hix.Pretty (prettyL, showPL)

logNonReinstallable :: NonEmpty PackageId -> M ()
logNonReinstallable ids =
  Log.verbose [exon|NOTE: Cabal solver suggested new versions for non-reinstallable packages: #{showPL ids}|]

-- | Forcing revisions means that any package that has a revision in the Hackage snapshot will be treated as an
-- override, i.e. it will be built from Hackage despite having the same version as the one installed in nixpkgs.
--
-- The benefit of doing this is that often nixpkgs will be outdated in comparison with Hackage, and therefore have
-- tighter dependency bounds.
-- When Cabal resolves a plan based on revised bounds in packages whose versions match nixpkgs, but not their revisions,
-- the nix build will fail with bounds errors, requiring a restart with revisions.
--
-- On the other hand, in many situations (like lower bound mutations), this is entirely irrelevant; in others, the
-- original bounds might just work for the current build; and most often nixpkgs actually has the latest revision, which
-- we cannot observe at this point.
-- Therefore, this feature is disabled until it can be refined.
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
    overrides = changes ++ forcedRevisions ++ reusedRevisions
    -- If a package has been selected for revision during a prior build, add it to the overrides despite its matching
    -- version.
    -- This simply ensures that the revision procedure can be skipped in this build, since the same version will likely
    -- cause the same dependency bounds error that triggered the revision.
    (reusedRevisions, installed) = partition (flip Set.member prevRevisions) noForcedRevisions
    (noForcedRevisions, forcedRevisions) = partitionEithers (checkRevision forceRevisions cabal <$> matching)
    mutablePIds = Set.fromList (depName <$> Set.toList deps.mutable)

-- TODO probably best to store the revisions in the SolverState
solveMutation ::
  CabalHandlers ->
  EnvDeps ->
  Set PackageId ->
  SolverState ->
  M (Maybe SolverChanges)
solveMutation cabal deps prevRevisions state =
  appContextVerbose "resolving a mutation build plan" do
    traverse (processSolverPlan state.flags.forceRevisions cabal deps prevRevisions) =<< cabal.solveForVersion state
