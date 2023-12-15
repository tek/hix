module Hix.Managed.State where

import qualified Data.Map.Strict as Map

import Hix.Class.Map (convertMaybe, ntInsert, via)
import Hix.Data.Bounds (Bounds)
import Hix.Data.Deps (TargetDeps)
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedEnv (ManagedEnv), ManagedEnvState (ManagedEnvState), ManagedState (..))
import Hix.Data.Overrides (Overrides)
import qualified Hix.Data.PackageId
import Hix.Data.Version (NewRange (NewRange, OldRange))
import Hix.Deps (distributeBounds)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpLowerInit))
import Hix.Version (lowerBound)

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
-- 'TargetDeps' is restricted to the targets by 'Hix.Managed.Data.ManagedJob.ManagedJob', so we don't need to filter
-- them here.
--
-- @lowerInit@ is the set of lower bounds that is preserved from LowerInit runs, so we only update them if that is the
-- current operation, by using the lower bounds of the new state.
--
-- TODO check whether it's really necessary to combine new lowerInit with the old ones, or if the new bounds contain all
-- deps.
envStateWithMutations ::
  ManagedOp ->
  EnvName ->
  TargetDeps ->
  ManagedState ->
  ManagedEnvState ->
  ManagedEnvState
envStateWithMutations op env deps new old =
  ManagedEnvState {
    bounds = distributeBounds new.bounds deps <> old.bounds,
    overrides = ntInsert env new.overrides old.overrides,
    lowerInit,
    resolving = old.resolving
  }
  where
    lowerInit | OpLowerInit <- op = via (Map.alter updateLowerInit env) old.lowerInit
              | otherwise = old.lowerInit

    updateLowerInit o = Just (newLowerInit <> fold o)

    newLowerInit = convertMaybe lowerBound new.bounds

envStateForBuild ::
  TargetDeps ->
  EnvName ->
  ManagedState ->
  ManagedEnvState
envStateForBuild deps env state =
  ManagedEnvState {
    bounds = distributeBounds state.bounds deps,
    overrides = [(env, state.overrides)],
    lowerInit = [],
    resolving = True
  }

envWithState ::
  ManagedEnvState ->
  ManagedEnv ->
  ManagedEnv
envWithState new ManagedEnv {..} =
  ManagedEnv {state = new, ..}

stateWithCandidate ::
  Bounds ->
  Candidate ->
  Overrides ->
  ManagedState
stateWithCandidate accBounds candidate overrides =
  ManagedState {bounds, overrides}
  where
    bounds = withCandidate accBounds

    withCandidate = case candidate.range of
      NewRange r -> ntInsert candidate.package.name r
      OldRange -> id
