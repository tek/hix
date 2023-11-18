module Hix.Managed.App where

import Hix.Class.Map ((!!))
import Hix.Data.Bounds (Bounds, UninitializedBounds, filterUninitialized)
import Hix.Data.Dep (Dep)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedEnv, ManagedState (ManagedState))
import Hix.Deps (allDeps, depsFromConfig, forTargets, mergeBounds, withManagedRanges)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.Build (BuildResult)
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
import Hix.Monad (M)

data ManagedApp =
  ManagedApp {
    build :: BuildHandlers,
    conf :: StateFileConfig,
    job :: ManagedJob,
    deps :: [Dep],
    state :: ManagedState,
    solverBounds :: Bounds,
    uninitialized :: UninitializedBounds
  }

managedJob ::
  ManagedEnv ->
  ManagedConfig ->
  M ManagedJob
managedJob env conf = do
  configDeps <- depsFromConfig env.deps env.targets
  let targets = sortTargets configDeps env.targets
      targetDeps = forTargets targets (withManagedRanges env.state.bounds configDeps)
  pure ManagedJob {env = conf.env, targetBound = conf.targetBound, ..}

managedApp ::
  BuildHandlers ->
  ManagedEnv ->
  ManagedConfig ->
  (ManagedApp -> M a) ->
  M a
managedApp build env conf use = do
  job <- managedJob env conf
  use ManagedApp {
    build,
    conf = conf.stateFile,
    job,
    deps = allDeps job.targetDeps,
    state = ManagedState {bounds = mergeBounds job.targetDeps, overrides},
    solverBounds = env.lower.solverBounds,
    uninitialized = filterUninitialized conf.targetBound job.targetDeps
  }
  where
    overrides = env.state.overrides !! conf.env

processAppResult ::
  ReportMutation a =>
  BuildHandlers ->
  ReportHandlers a ->
  ManagedEnv ->
  ManagedConfig ->
  ManagedApp ->
  Either [DepMutation a] (BuildResult a) ->
  M ()
processAppResult build report env conf app = \case
  Right result ->
    updateProject build.stateFile report conf.stateFile app.job app.uninitialized env.state result
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
    processAppResult build report env conf app result
