module Hix.Managed.EnvRequest where

import Control.Lens ((%~))
import Distribution.Pretty (pretty)
import Exon (exon)

import Hix.Class.Map (nFlatten, nMap, nRestrictKeys, (!!))
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.VersionBounds (majorRange)
import qualified Hix.Log as Log
import Hix.Managed.Data.Diff (Change (Unchanged))
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import qualified Hix.Managed.Data.EnvRequest
import Hix.Managed.Data.EnvRequest (EnvRequest (EnvRequest))
import qualified Hix.Managed.Data.EnvResult
import Hix.Managed.Data.EnvResult (EnvResult)
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState (EnvState))
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (mutReplace)
import qualified Hix.Managed.Data.ProcessState
import Hix.Managed.Data.ProcessState (ProcessState (ProcessState))
import qualified Hix.Managed.Data.ProjectState
import Hix.Managed.Data.ProjectState (ProjectState)
import Hix.Managed.Data.Targets (overTargets, targetsSet)
import Hix.Managed.Diff (initChanges, reifyBoundsChange)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers, Builder, runBuilder)
import Hix.Managed.UpdateState (projectStateWithEnv)

initialEnvState ::
  EnvContext ->
  ProjectState ->
  Initial EnvState
initialEnvState context projectState =
  Initial EnvState {..}
  where
    bounds = nFlatten (Unchanged . Just) (nRestrictKeys (targetsSet context.targets) projectState.bounds)
    versions = initChanges (projectState.versions !! context.env)
    overrides = projectState.overrides !! context.env
    initial = initChanges (projectState.initial !! context.env)

updateProcessState ::
  EnvContext ->
  Initial EnvState ->
  EnvResult ->
  ProcessState ->
  ProcessState
updateProcessState context (Initial initialState) envResult ProcessState {state = projectState, packages} = do
  ProcessState {
    packages = overTargets context.targets replaceBounds packages,
    state = projectStateWithEnv context final projectState
  }
  where
    replaceBounds = #mutable %~ mutReplace ranges
    ranges = nMap (majorRange . reifyBoundsChange) final.bounds
    final = fromMaybe initialState envResult.state

withEnvRequest ::
  BuildHandlers ->
  ProcessState ->
  EnvContext ->
  Builder ->
  (EnvRequest -> M EnvResult) ->
  M (ProcessState, EnvResult)
withEnvRequest build state context builder use = do
  cabal <- build.cabal state.packages context.ghc
  envResult <- runBuilder builder cabal context initialState \ envBuilder ->
    use EnvRequest {context, builder = envBuilder, state = initialState}
  let newState = updateProcessState context initialState envResult state
  Log.debug [exon|Finished '##{context.env :: EnvName}' with final state:|]
  Log.debugP (pretty newState)
  pure (newState, envResult)
  where
    initialState = initialEnvState context state.state
