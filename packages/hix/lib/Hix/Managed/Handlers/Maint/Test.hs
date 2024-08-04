module Hix.Managed.Handlers.Maint.Test where

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (ManagedOptions)
import Hix.Http (httpManager)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish))
import Hix.Managed.Data.BuildConfig (SpecialBuildHandlers (BuildHandlersTestMaint))
import Hix.Managed.Data.BuildOutput (DepChanges)
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.ProjectContext (ProjectContext (..))
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Build.Test as Build
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import Hix.Managed.Handlers.Context (ContextHandlers (..), ContextKey (ContextManaged), ContextQuery (ContextQuery))
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.Maint (MaintHandlers (..))
import Hix.Managed.Handlers.Maint.Prod (projectWithEnv, runBump)
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.Maint.Git (gitApiMaintProd)
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto

bumpHandlers ::
  ContextHandlers ->
  ManagedOptions ->
  EnvName ->
  M (BuildHandlers, ProjectContext)
bumpHandlers ContextHandlers {query} options envName = do
  proto <- query (ContextQuery ContextManaged)
  project <- ProjectContextProto.validate options.project proto
  handlersProject <- Project.handlersProd options.stateFile
  handlers <-
    Build.chooseHandlers (Just BuildHandlersTestMaint) handlersProject proto.envs options.project.build project.cabal
  bumpProject <- projectWithEnv envName project
  pure (handlers, bumpProject)

runBumpTest ::
  ContextHandlers ->
  ManagedOptions ->
  EnvName ->
  M DepChanges
runBumpTest config options envName = do
  (handlers, project) <- bumpHandlers config options envName
  runBump handlers project

handlersTest ::
  ManagedOptions ->
  MaintConfig ->
  CabalConfig ->
  M MaintHandlers
handlersTest options config cabal = do
  manager <- httpManager
  publishHackages <- HackageClient.handlersProdFor (Just manager) ForPublish cabal
  pure MaintHandlers {
    git = gitApiMaintProd config,
    context,
    runBump = runBumpTest context options,
    publishHackages
  }
  where
    context = ContextHandlers.handlersProd
