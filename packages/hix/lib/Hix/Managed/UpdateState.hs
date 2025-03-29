module Hix.Managed.UpdateState where

import Hix.Class.Map (nAdjust, nAmend, nInsert, nMapKeys)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Overrides (Overrides)
import Hix.Managed.Data.Diff (BoundsChange)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState (EnvState))
import Hix.Managed.Data.Mutable (depName)
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState)
import Hix.Managed.Data.Packages (Deps)
import qualified Hix.Managed.Data.ProjectState
import Hix.Managed.Data.ProjectState (ProjectState (ProjectState))
import Hix.Managed.Diff (applyBoundsChange, applyVersionChange, updateBoundsChanges, updateVersionChanges)
import Hix.Managed.Targets (overTargets)

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
-- in 'envStateWithMutations'.
projectStateWithEnv ::
  EnvContext ->
  EnvState ->
  ProjectState ->
  ProjectState
projectStateWithEnv context new old =
  ProjectState {
    -- TODO clean up
    -- bounds = mutUpdateTargets context.targets applyBoundsChange new.bounds old.bounds,
    bounds = overTargets context.targets (nAmend applyBoundsChange depsBounds) old.bounds,
    versions = nAdjust context.env old.versions (nAmend applyVersionChange new.versions),
    initial = nAdjust context.env old.initial (nAmend applyVersionChange new.initial),
    overrides = nInsert context.env new.overrides old.overrides,
    solver = nInsert context.env new.solver old.solver,
    resolving = old.resolving
  }
  where
    depsBounds = nMapKeys depName new.bounds :: Deps BoundsChange

envStateForBuild ::
  EnvName ->
  Overrides ->
  ProjectState
envStateForBuild env overrides =
  ProjectState {
    bounds = mempty,
    versions = mempty,
    initial = [],
    overrides = [(env, overrides)],
    solver = [],
    resolving = True
  }

envStateForSolver ::
  EnvName ->
  Overrides ->
  ProjectState
envStateForSolver env overrides =
  ProjectState {
    bounds = mempty,
    versions = mempty,
    initial = [],
    overrides = [],
    solver = [(env, overrides)],
    resolving = True
  }

-- TODO can MutationState contain changes for @initial@?
envStateWithMutations ::
  MutationState ->
  EnvState ->
  EnvState
envStateWithMutations new EnvState {bounds, versions, initial, solver} =
  EnvState {
    bounds = updateBoundsChanges new.bounds bounds,
    versions = updateVersionChanges new.versions versions,
    initial = updateVersionChanges new.initial initial,
    overrides = new.overrides,
    solver
  }
