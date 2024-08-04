module Hix.Managed.Handlers.Maint.Prod where

import qualified Data.List.NonEmpty as NonEmpty
import Exon (exon)
import Path (Abs, Dir, Path)

import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.Monad
import Hix.Data.Monad (M, appRes)
import Hix.Data.Options (ManagedOptions (..), ProjectOptions (..))
import Hix.Http (httpManager)
import Hix.Managed.BuildOutput (depChanges)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish))
import Hix.Managed.Data.BuildOutput (BuildOutput, DepChanges)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.ProjectContext (ProjectContext (..))
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Flake (flakeFailure, runFlake, runFlakeFor)
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import Hix.Managed.Handlers.Context (ContextHandlers (..), ContextKey (ContextManaged), queryContext)
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.Maint (MaintHandlers (..))
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.Maint.Git (gitApiMaintHermetic, gitApiMaintProd)
import Hix.Managed.ProjectContext (updateProject)
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto
import Hix.Monad (noteFatal)

-- TODO remove
runBumpFlake :: Path Abs Dir -> M BuildOutput
runBumpFlake root = runFlake "Managed bounds" root ["run", ".#bump", "--", "--output=json", "--handlers=test-maint", "--build-output"] id

projectWithEnv :: EnvName -> ProjectContext -> M ProjectContext
projectWithEnv target ProjectContext {..} = do
  newEnvs <- noteFatal [exon|Couldn't find env '##{target}' in the project context|] (nonEmpty updated)
  pure ProjectContext {envs = newEnvs, ..}
  where
    updated = flip NonEmpty.filter envs \case
      Left name -> name == target
      Right context -> context.env == target

bumpHandlers ::
  ContextHandlers ->
  ManagedOptions ->
  EnvName ->
  M (BuildHandlers, ProjectContext)
bumpHandlers contexts options envName = do
  proto <- queryContext contexts ContextManaged
  project <- ProjectContextProto.validate options.project proto
  handlersProject <- Project.handlersProd options.stateFile
  handlers <- Build.handlersProd handlersProject proto.envs options.project.build project.cabal
  bumpProject <- projectWithEnv envName project
  pure (handlers, bumpProject)

runBump ::
  BuildHandlers ->
  ProjectContext ->
  M DepChanges
runBump handlers project = do
  result <- bumpOptimizeMain handlers project
  updateProject handlers.project False result
  root <- appRes.root
  -- TODO maybe the called app should be aware of managed, and check the config to ensure that the right steps are
  -- taken.
  runFlakeFor (const unit) flakeFailure "Generate Cabal" root ["--quiet", "--quiet", "--quiet", "run", ".#gen-quiet"] id
  pure (depChanges result)

runBumpProd ::
  ContextHandlers ->
  ManagedOptions ->
  EnvName ->
  M DepChanges
runBumpProd context options envName = do
  (handlers, project) <- bumpHandlers context options envName
  runBump handlers project

handlersProd ::
  ManagedOptions ->
  MaintConfig ->
  CabalConfig ->
  M MaintHandlers
handlersProd options config cabal = do
  manager <- httpManager
  publishHackages <- HackageClient.handlersProdFor (Just manager) ForPublish cabal
  pure MaintHandlers {
    git = if config.ci then gitApiMaintHermetic config else gitApiMaintProd config,
    context,
    runBump = runBumpProd context options,
    publishHackages
  }
  where
    context = ContextHandlers.handlersProd
