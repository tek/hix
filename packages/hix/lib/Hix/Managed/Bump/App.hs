module Hix.Managed.Bump.App where

import Control.Monad (foldM)

import Hix.Data.Error (Error (Client))
import qualified Hix.Data.ManagedEnv
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (BumpOptions)
import Hix.Json (jsonConfigE)
import qualified Hix.Managed.App
import Hix.Managed.App (ManagedApp, runManagedApp)
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Env (BuildEnv, withBuildEnv)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Bump.Candidates (candidatesBump)
import Hix.Managed.Data.Build (BuildResult, BuildResults, initBuildResults, updateBuildResults)
import qualified Hix.Managed.Data.ManagedConfig
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers, SpecialBumpHandlers (TestBumpHandlers))
import Hix.Managed.Handlers.Bump.Prod (handlersProd)
import Hix.Managed.Handlers.Bump.Test (handlersTest)
import Hix.Managed.Handlers.Mutation.Bump (handlersBump)
import qualified Hix.Managed.Lower.Data.Bump
import Hix.Managed.Lower.Data.Bump (Bump, BumpState (BumpState))

bumpJob ::
  BumpHandlers ->
  BuildEnv ->
  ManagedJob ->
  M (BuildResult Bump)
bumpJob handlers buildEnv job = do
  mutations <- candidatesBump handlers job.deps
  buildMutations buildEnv handlersBump job job.state mutations initialState
  where
    initialState = BumpState {overrides = job.state.overrides}

bumpBuild ::
  BumpHandlers ->
  ManagedApp ->
  M (BuildResults Bump)
bumpBuild handlers app = do
  withBuildEnv app.build app.conf \ buildEnv -> do
    let
      build results job = do
        res <- bumpJob handlers buildEnv job
        pure (updateBuildResults job.env job.targetDeps results res)
    foldM build (initBuildResults app.state) app.jobs

bumpReport ::
  BumpHandlers ->
  ManagedApp ->
  M [DepMutation Bump]
bumpReport handlers app = do
  mutations <- for app.jobs \ job -> candidatesBump handlers job.deps
  pure (join (toList mutations))

bump ::
  BumpHandlers ->
  ManagedApp ->
  M (Either [DepMutation Bump] (BuildResults Bump))
bump handlers app
  | app.conf.updateProject
  = Right <$> bumpBuild handlers app
  | otherwise
  = Left <$> bumpReport handlers app

chooseHandlers ::
  Maybe Text ->
  Maybe SpecialBumpHandlers ->
  IO (BumpHandlers)
chooseHandlers buildOutputsPrefix = \case
  Just TestBumpHandlers -> handlersTest buildOutputsPrefix
  Nothing -> handlersProd buildOutputsPrefix

bumpCli :: BumpOptions -> M ()
bumpCli opts = do
  env <- jsonConfigE Client opts.env
  handlers <- liftIO (chooseHandlers env.buildOutputsPrefix opts.handlers)
  runManagedApp handlers.build handlers.report env opts.config \ app ->
    bump handlers app
