module Hix.Managed.State where

import Hix.Class.Map (ntInsert)
import Hix.Data.Bounds (Bounds)
import Hix.Data.Deps (TargetDeps)
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedEnv (ManagedEnv), ManagedEnvState (ManagedEnvState), ManagedState (..))
import Hix.Data.Overrides (EnvOverrides (EnvOverrides), Override, Overrides)
import Hix.Data.Package (PackageName)
import qualified Hix.Data.Version
import Hix.Data.Version (NewRange (NewRange, OldRange))
import Hix.Deps (distributeBounds)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)

-- | Insert the transformed 'ManagedState', which contains only the bounds for the current target set and the overrides
-- for the current env.
--
-- Since the env is intended to be exclusively used with its associated target set, we simply overwrite the existing one
-- completely.
--
-- Bounds are supposed to be universally valid, since they qualify to the source rather than a build configuration, so
-- we have to combine them with the potential bounds sets of other target sets by taking the left-biased union (@<>@).
-- Since the build state uses @Bounds@ instead of a target-keyed nested @Map@, we also have to create the latter via
-- 'distributeBounds'.
--
-- 'TargetDeps' is restricted to the targets by 'Hix.Managed.App.ManagedApp', so we don't need to filter them here.
envStateWithOverrides ::
  EnvName ->
  TargetDeps ->
  ManagedState ->
  ManagedEnvState ->
  ManagedEnvState
envStateWithOverrides env deps new old =
  ManagedEnvState {
    bounds = distributeBounds new.bounds deps <> old.bounds,
    overrides = ntInsert env new.overrides old.overrides,
    resolving = old.resolving
  }

envWithOverrides ::
  EnvName ->
  TargetDeps ->
  ManagedState ->
  ManagedEnv ->
  ManagedEnv
envWithOverrides env deps' new ManagedEnv {..} =
  ManagedEnv {state = envStateWithOverrides env deps' new state, ..}

envWithState ::
  ManagedEnvState ->
  ManagedEnv ->
  ManagedEnv
envWithState new ManagedEnv {..} =
  ManagedEnv {state = new, ..}

managedWithCandidate ::
  Bounds ->
  Candidate ->
  Overrides ->
  ManagedState
managedWithCandidate accBounds candidate overrides =
  ManagedState {bounds, overrides}
  where
    bounds = withCandidate accBounds

    withCandidate = case candidate.range of
      NewRange r -> ntInsert candidate.version.package r
      OldRange -> id

managedWithOverride ::
  PackageName ->
  Override ->
  ManagedState ->
  ManagedState
managedWithOverride package override state =
  state {overrides = ntInsert package override state.overrides}

managedEnvForBuild ::
  ManagedJob ->
  ManagedState ->
  ManagedEnvState
managedEnvForBuild job state =
  ManagedEnvState {
    bounds = distributeBounds state.bounds job.targetDeps,
    overrides = EnvOverrides [(job.env, state.overrides)],
    resolving = True
  }

managedEnvForProject ::
  ManagedJob ->
  ManagedEnvState ->
  ManagedState ->
  ManagedEnvState
managedEnvForProject job old new =
  envStateWithOverrides job.env job.targetDeps new old
