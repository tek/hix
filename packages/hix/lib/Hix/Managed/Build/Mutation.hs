module Hix.Managed.Build.Mutation where

import Control.Lens ((.~))
import Exon (exon)

import Hix.Class.Map (nAdjust, nAmendWithKey)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (VersionBounds, exactVersion)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.SolverState (SolverState, updateSolverState)
import Hix.Managed.Data.Constraints (EnvConstraints, MutationConstraints)
import Hix.Managed.Data.Mutable (MutableDep, depName, mutRelax)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (BuildMutation (BuildMutation))
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState)
import Hix.Managed.Data.Packages (Deps)
import Hix.Pretty (showP)

candidateMutation ::
  EnvConstraints ->
  MutableId ->
  (Version -> VersionBounds -> VersionBounds) ->
  BuildMutation
candidateMutation constraints candidate updateBound =
  BuildMutation {
    description = showP candidate,
    constraints,
    updateBound
  }

candidateConstraints :: EnvConstraints -> MutableId -> EnvConstraints
candidateConstraints constraints MutableId {..} =
  nAdjust (depName name) constraints (#mutation .~ exactVersion version)

updateConstraints ::
  (MutableId -> PackageId -> MutationConstraints -> MutationConstraints) ->
  MutableId ->
  MutationState ->
  EnvConstraints ->
  EnvConstraints
updateConstraints impl candidate state =
  nAmendWithKey update (mutRelax state.versions :: Deps (Maybe Version))
  where
    update name = \case
      Just version -> impl candidate PackageId {..}
      Nothing -> id

-- | Boilerplate for mutation handlers, could be better.
--
-- TODO If we'd use the @retract@ field from @DepMutation@ and the target bound here, we could probably use a universal
-- bounds updater without leaking implementation...investigate.
buildCandidate ::
  (BuildMutation -> M (Maybe MutationState)) ->
  (Version -> VersionBounds -> VersionBounds) ->
  (MutableId -> PackageId -> MutationConstraints -> MutationConstraints) ->
  SolverState ->
  MutableDep ->
  Version ->
  M (Maybe (MutableId, SolverState, MutationState))
buildCandidate build updateStateBound updateConstraintBound solverState package version = do
  Log.debug [exon|Mutation constraints for #{showP candidate}: #{showP constraints}|]
  fmap result <$> build (candidateMutation constraints candidate updateStateBound)
  where
    result newState = (candidate, newSolverState newState, newState)

    candidate = MutableId {name = package, version}

    constraints = candidateConstraints solverState.constraints candidate

    newSolverState newState =
      updateSolverState (updateConstraints updateConstraintBound candidate newState) solverState
