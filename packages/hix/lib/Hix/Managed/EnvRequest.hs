module Hix.Managed.EnvRequest where

import Distribution.Pretty (pretty)
import Exon (exon)

import Hix.Class.Map (nFlatten, nMap, (!!))
import Hix.Data.Bounds (Ranges)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.VersionBounds (majorRange)
import qualified Hix.Log as Log
import Hix.Managed.Data.Diff (BoundsChange, Change (Unchanged))
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import qualified Hix.Managed.Data.EnvRequest
import Hix.Managed.Data.EnvRequest (EnvRequest (EnvRequest))
import qualified Hix.Managed.Data.EnvResult
import Hix.Managed.Data.EnvResult (EnvResult)
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState (EnvState))
import Hix.Managed.Data.Initial (Initial (Initial))
import qualified Hix.Managed.Data.Mutable as Mutable
import Hix.Managed.Data.Mutable (MutableBounds, MutableDeps, MutableRanges, mutRelax)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProcessState
import Hix.Managed.Data.ProcessState (ProcessState (ProcessState))
import qualified Hix.Managed.Data.ProjectState
import Hix.Managed.Data.ProjectState (ProjectState)
import Hix.Managed.Diff (initChanges, reifyBoundsChange)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers, Builder, EnvBuilderContext (..), runBuilder)
import Hix.Managed.ManagedPackage (updateRanges)
import Hix.Managed.Targets (overTargets)
import Hix.Managed.UpdateState (projectStateWithEnv)

initialEnvState ::
  EnvContext ->
  ProjectState ->
  Initial EnvState
initialEnvState context projectState =
  Initial EnvState {..}
  where
    bounds :: MutableDeps BoundsChange
    bounds = nFlatten (Unchanged . Just) mutableBounds

    -- TODO this has the consequence that after this env is done, only the mutable bounds will be written to the state,
    -- deleting any local deps.
    -- This is due to the principle I established at some point that the state should _always_ contain all dependencies.
    mutableBounds :: Packages MutableBounds
    mutableBounds = Mutable.forTargets context.targets projectState.bounds

    versions = initChanges (projectState.versions !! context.env)

    overrides = projectState.overrides !! context.env

    solver = projectState.solver !! context.env

    initial = initChanges (projectState.initial !! context.env)

updateProcessState ::
  EnvContext ->
  Initial EnvState ->
  EnvResult ->
  ProcessState ->
  ProcessState
updateProcessState context (Initial initialState) envResult ProcessState {packages, state = projectState} = do
  ProcessState {
    packages = overTargets context.targets replaceBounds packages,
    state = projectStateWithEnv context final projectState
  }
  where
    replaceBounds = updateRanges \ package original -> fromMaybe original (ranges !! package)
    ranges :: Ranges
    ranges = mutRelax (nMap (majorRange . reifyBoundsChange) final.bounds :: MutableRanges)
    final = fromMaybe initialState envResult.state

withEnvRequest ::
  BuildHandlers ->
  ProcessState ->
  EnvContext ->
  Builder ->
  (EnvRequest -> M EnvResult) ->
  M (ProcessState, EnvResult)
withEnvRequest build state context@env builder use = do
  envResult <- runBuilder builder envBuilderContext \ envBuilder ->
    use EnvRequest {context, builder = envBuilder, state = initialState}
  let newState = updateProcessState context initialState envResult state
  Log.debug [exon|Finished '##{context.env :: EnvName}' with final state:|]
  Log.debugP (pretty newState)
  pure (newState, envResult)
  where
    envBuilderContext = EnvBuilderContext {..}

    initCabal = build.cabal state.packages

    initialState = initialEnvState context state.state
