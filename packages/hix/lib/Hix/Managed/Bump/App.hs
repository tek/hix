module Hix.Managed.Bump.App where

import Hix.Data.Error (Error (Client))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (BuildOutputsPrefix, EnvsConfig)
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (BumpOptions)
import Hix.Json (jsonConfigE)
import Hix.Managed.App (runManagedApp)
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Bump.Candidates (candidatesBump)
import Hix.Managed.Data.BuildResult (BuildResult, buildResult)
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.BuildState (initBuildState)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpBump), StateFileConfig)
import qualified Hix.Managed.Data.ManagedJob
import qualified Hix.Managed.Data.ManagedJob as ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (Builder)
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers, SpecialBumpHandlers (TestBumpHandlers))
import Hix.Managed.Handlers.Bump.Prod (handlersProd)
import Hix.Managed.Handlers.Bump.Test (handlersTest)
import Hix.Managed.Handlers.Mutation.Bump (handlersBump)
import Hix.Managed.Job (buildJobs)
import qualified Hix.Managed.Lower.Data.Bump
import Hix.Managed.Lower.Data.Bump (Bump, BumpState (BumpState))

bumpJob ::
  BumpHandlers ->
  Builder ->
  ManagedJob ->
  M (BuildResult Bump)
bumpJob handlers builder job = do
  mutations <- candidatesBump handlers job.query
  result <- buildMutations handlers.build.hackage builder mutationHandlers job mutations state
  pure (buildResult job.removable result)
  where
    mutationHandlers _ = pure handlersBump
    state = initBuildState (ManagedJob.initialState job) ext
    ext = BumpState {overrides = job.overrides}

bumpReport ::
  BumpHandlers ->
  ManagedApp ->
  M [DepMutation Bump]
bumpReport handlers app = do
  mutations <- for app.jobs \ job -> candidatesBump handlers job.query
  pure (join (toList mutations))

bump ::
  BumpHandlers ->
  ManagedApp ->
  M (Either [DepMutation Bump] (BuildResults Bump))
bump handlers app
  | app.conf.updateProject
  = Right <$> buildJobs app (bumpJob handlers)
  | otherwise
  = Left <$> bumpReport handlers app

chooseHandlers ::
  StateFileConfig ->
  EnvsConfig ->
  Maybe BuildOutputsPrefix ->
  Maybe SpecialBumpHandlers ->
  IO (BumpHandlers)
chooseHandlers stateFileConf envsConf buildOutputsPrefix = \case
  Just TestBumpHandlers -> handlersTest stateFileConf envsConf buildOutputsPrefix
  Nothing -> handlersProd stateFileConf envsConf buildOutputsPrefix

bumpCli :: BumpOptions -> M ()
bumpCli opts = do
  env <- jsonConfigE Client opts.env
  handlers <- liftIO (chooseHandlers opts.config.stateFile env.envs env.buildOutputsPrefix opts.handlers)
  runManagedApp handlers.build handlers.report env opts.config OpBump \ app ->
    bump handlers app
