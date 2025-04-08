module Hix.Managed.Process where

import Data.Generics.Labels ()
import Distribution.Pretty (pretty)
import Exon (exon)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import qualified Hix.Log as Log
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import qualified Hix.Managed.Data.EnvRequest
import qualified Hix.Managed.Data.EnvResult
import Hix.Managed.Data.EnvResult (EnvResult (EnvResult))
import qualified Hix.Managed.Data.ProcessState
import Hix.Managed.Data.ProcessState (ProcessState, initProcessState)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext)
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Data.ProjectResult (ProjectResult (ProjectResult))
import Hix.Managed.Data.StageResult (StageSummary (StageNoAction))
import Hix.Managed.EnvRequest (withEnvRequest)
import Hix.Managed.Flow (Flow, execFlow)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers (BuildHandlers), Builder)
import Hix.Monad (mapAccumM)

processEnv ::
  BuildHandlers ->
  Flow () ->
  Builder ->
  ProcessState ->
  EnvContext ->
  M (ProcessState, EnvResult)
processEnv build process builder state context = do
  withEnvRequest build state context builder \ request -> do
    Log.debug [exon|Processing env '##{context.env :: EnvName}' with initial state:|]
    Log.debugP (pretty request.state)
    execFlow request process

skipResult :: StageSummary
skipResult =
  StageNoAction (Just "This environment has no manageable dependencies.")

processOrSkip ::
  (ProcessState -> EnvContext -> M (ProcessState, EnvResult)) ->
  ProcessState ->
  Either EnvName EnvContext ->
  M (ProcessState, EnvResult)
processOrSkip process state = \case
  Right context -> process state context
  Left env -> pure (state, EnvResult {env, state = Nothing, summaries = [skipResult]})

foldEnvs ::
  ProjectContext ->
  (ProcessState -> EnvContext -> M (ProcessState, EnvResult)) ->
  M ProjectResult
foldEnvs project process = do
  (final, envs) <- mapAccumM (processOrSkip process) (initProcessState project) project.envs
  pure ProjectResult {envs, state = final.state}

processProject ::
  BuildHandlers ->
  ProjectContext ->
  Flow () ->
  M ProjectResult
processProject build@BuildHandlers {withBuilder} project flow =
  withBuilder \ builder -> foldEnvs project (processEnv build flow builder)

processProjectSimple ::
  (BuildHandlers -> BuildConfig -> Flow a) ->
  BuildHandlers ->
  ProjectContext ->
  M ProjectResult
processProjectSimple flow handlers project =
  processProject handlers project (void (flow handlers project.build))
