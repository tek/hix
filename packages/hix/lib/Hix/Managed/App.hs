module Hix.Managed.App where

import Control.Monad.Trans.Reader (asks)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString

import Hix.Class.Map ((!!))
import Hix.Data.Bounds (Bounds, RemovableBounds, removableBounds)
import Hix.Data.Dep (Dep)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedEnv, ManagedState (ManagedState))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M)
import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Deps (allDeps, depsFromConfig, forTargets, mergeBounds, withManagedRanges)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.Build (BuildResult, buildOutput)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig, StateFileConfig)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob (ManagedJob))
import Hix.Managed.Data.Targets (sortTargets)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Report (ReportHandlers)
import Hix.Managed.Project (updateProject)
import Hix.Managed.Report (ReportMutation, reportMutations)

data ManagedApp =
  ManagedApp {
    build :: BuildHandlers,
    conf :: StateFileConfig,
    job :: ManagedJob,
    deps :: [Dep],
    state :: ManagedState,
    solverBounds :: Bounds,
    -- | The deps in each target that had a 'targetBound' in the flake config, but not in the managed state.
    -- For the purpose of informing the user that they can be removed.
    removable :: RemovableBounds
  }

managedJob ::
  ManagedEnv ->
  ManagedConfig ->
  M (RemovableBounds, ManagedJob)
managedJob env conf = do
  configDeps <- depsFromConfig env.deps env.targets
  let targets = sortTargets configDeps env.targets
      targetDeps = forTargets targets (withManagedRanges env.state.bounds configDeps)
      removable = removableBounds conf.targetBound targetDeps env.state.bounds
  pure (removable, ManagedJob {env = conf.env, targetBound = conf.targetBound, ..})

managedApp ::
  BuildHandlers ->
  ManagedEnv ->
  ManagedConfig ->
  (ManagedApp -> M a) ->
  M a
managedApp build env conf use = do
  (removable, job) <- managedJob env conf
  use ManagedApp {
    build,
    conf = conf.stateFile,
    job,
    deps = allDeps job.targetDeps,
    state = ManagedState {bounds = mergeBounds job.targetDeps, overrides},
    solverBounds = env.lower.solverBounds,
    removable
  }
  where
    overrides = env.state.overrides !! conf.env

outputResult :: BuildResult a -> OutputFormat -> M ()
outputResult result = \case
  OutputNone -> unit
  OutputJson -> liftIO (ByteString.putStrLn (Aeson.encode (buildOutput result)))

processAppResult ::
  ReportMutation a =>
  ReportHandlers a ->
  ManagedEnv ->
  ManagedConfig ->
  ManagedApp ->
  Either [DepMutation a] (BuildResult a) ->
  M ()
processAppResult report env conf app = \case
  Right result -> do
    updateProject app.build.stateFile report conf.stateFile app.job app.removable env.state result
    outputResult result =<< asks (.output)
  Left mutations ->
    reportMutations mutations

runManagedApp ::
  ReportMutation a =>
  BuildHandlers ->
  ReportHandlers a ->
  ManagedEnv ->
  ManagedConfig ->
  (ManagedApp -> M (Either [DepMutation a] (BuildResult a))) ->
  M ()
runManagedApp build report env conf use =
  managedApp build env conf \ app -> do
    result <- use app
    processAppResult report env conf app result
