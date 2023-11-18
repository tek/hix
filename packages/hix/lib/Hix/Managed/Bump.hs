module Hix.Managed.Bump where

import Hix.Data.Error (Error (Client))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState)
import qualified Hix.Data.Options
import Hix.Data.Options (BumpOptions)
import Hix.Json (jsonConfigE)
import qualified Hix.Managed.App
import Hix.Managed.App (ManagedApp, runManagedApp)
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Bump.Candidates (candidatesBump)
import Hix.Managed.Data.Build (BuildResult)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Data.ManagedJob (ManagedJob)
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers, SpecialBumpHandlers (TestBumpHandlers))
import Hix.Managed.Handlers.Bump.Prod (handlersProd)
import Hix.Managed.Handlers.Bump.Test (handlersTest)
import Hix.Managed.Handlers.Mutation.Bump (handlersBump)
import qualified Hix.Managed.Lower.Data.Bump
import Hix.Managed.Lower.Data.Bump (Bump, BumpState (BumpState))
import Hix.Monad (M)

buildAndUpdate ::
  BumpHandlers ->
  StateFileConfig ->
  ManagedJob ->
  ManagedState ->
  [DepMutation Bump] ->
  M (BuildResult Bump)
buildAndUpdate handlers conf job managed mutations =
  buildMutations handlers.build handlersBump conf job managed mutations initialState
  where
    initialState = BumpState {overrides = managed.overrides}

bumpDeps ::
  BumpHandlers ->
  ManagedApp ->
  M (Either [DepMutation Bump] (BuildResult Bump))
bumpDeps handlers app = do
  mutations <- candidatesBump handlers app.deps
  if app.conf.updateProject
  then Right <$> buildAndUpdate handlers app.conf app.job app.state mutations
  else pure (Left (toList mutations))

chooseHandlers :: Maybe SpecialBumpHandlers -> IO (BumpHandlers)
chooseHandlers = \case
  Just TestBumpHandlers -> handlersTest
  Nothing -> handlersProd

bumpCli :: BumpOptions -> M ()
bumpCli opts = do
  env <- jsonConfigE Client opts.env
  handlers <- liftIO (chooseHandlers opts.handlers)
  runManagedApp handlers.build handlers.report env opts.config \ app ->
    bumpDeps handlers app
