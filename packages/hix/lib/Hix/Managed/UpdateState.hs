module Hix.Managed.UpdateState where

import Hix.Class.Map (nAdjust, nAmend, nInsert)
import Hix.Data.Overrides (Overrides)
import Hix.Managed.Data.Diff (Change (Changed, Unchanged), Diff (DiffAdded), VersionChange)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState (EnvState))
import Hix.Managed.Data.Mutable (mutUpdateTargets)
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState)
import qualified Hix.Managed.Data.ProjectState
import Hix.Managed.Data.ProjectState (ProjectState (ProjectState))
import Hix.Managed.Diff (applyBoundsChange, applyVersionChange, updateBoundsChanges, updateVersionChanges)

-- | Insert the transformed 'EnvState', which contains only the bounds for the current target set and the overrides
-- for the current env.
--
-- All data is stored as diffs in 'EnvState' (except for overrides), which are applied to the initial state.
--
-- Since the env is intended to be exclusively used with its associated target set, we simply overwrite the existing one
-- completely.
--
-- Bounds are supposed to be universally valid, since they qualify the source rather than a build configuration, so they
-- have to be updated in each of the env's target packages.
--
-- @initial@ is the set of bounds that is preserved from the first run for each dependency.
-- The 'EnvState' only contains updates for them if an update for @versions@ contains @DiffAdded@, which is handled
-- in 'stateWithMutations'.
projectStateWithEnv ::
  EnvContext ->
  EnvState ->
  ProjectState ->
  ProjectState
projectStateWithEnv context new old =
  ProjectState {
    bounds = mutUpdateTargets context.targets applyBoundsChange new.bounds old.bounds,
    versions = nAdjust context.env old.versions (nAmend applyVersionChange new.versions),
    overrides = nInsert context.env new.overrides old.overrides,
    initial = nAdjust context.env old.initial (nAmend applyVersionChange new.initial),
    resolving = old.resolving
  }

envStateForBuild ::
  EnvContext ->
  Overrides ->
  ProjectState
envStateForBuild context overrides =
  ProjectState {
    bounds = mempty,
    versions = mempty,
    overrides = [(context.env, overrides)],
    initial = [],
    resolving = True
  }

-- | Initial versions have the purpose of retaining the build plan least likely to be broken by changes for lower
-- bounds.
--
-- @lower.init@ selects the highest buildable version for the oldest GHC and is supposed to be run before all other
-- stages.
-- Since the persisted state is empty in the beginning, this can be detected by the diffs of the mutation versions – if
-- the new version was added (as opposed to updated), it is copied to the initial versions.
-- This also matches when the current initial version is absent, for the case where the initial state was tampered with
-- (used in a test).
updateInitial :: VersionChange -> VersionChange -> VersionChange
updateInitial new@(Changed (DiffAdded _)) _ = new
updateInitial new (Unchanged Nothing) = new
updateInitial _ old = old

stateWithMutations ::
  MutationState ->
  EnvState ->
  EnvState
stateWithMutations new EnvState {bounds, versions = oldVersions, initial} =
  EnvState {
    bounds = updateBoundsChanges new.bounds bounds,
    versions,
    overrides = new.overrides,
    initial = nAmend updateInitial versions initial
  }
  where
    versions = updateVersionChanges new.versions oldVersions
