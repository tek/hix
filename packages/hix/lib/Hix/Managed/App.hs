module Hix.Managed.App where

import Control.Monad.Trans.Reader (ask)
import qualified Data.Map.Strict as Map
import Exon (exon)

import Hix.Class.Map (ntMap, (!!))
import Hix.Data.Bounds (TargetBound (TargetLower, TargetUpper), removableBounds)
import Hix.Data.Deps (targetRemoteDeps)
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (EnvConfig, ManagedEnv)
import qualified Hix.Data.Monad
import Hix.Data.Monad (M)
import Hix.Deps (depsFromConfig, forTargets, uniqueRemoteDeps, withManagedRanges)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.BuildOutput (outputResult)
import Hix.Managed.Data.BuildOutput (buildOutput)
import Hix.Managed.Data.BuildResults (BuildResults)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp (ManagedApp))
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (ManagedConfig)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob (ManagedJob))
import Hix.Managed.Data.ManagedOp (ManagedOp (OpBump, OpLowerInit, OpLowerOptimize, OpLowerStabilize))
import Hix.Managed.Data.Targets (sortTargets)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Report (ReportHandlers)
import Hix.Managed.Project (updateProject)
import Hix.Managed.Report (ReportMutation, reportMutations)
import Hix.Monad (Env (Env), noteClient)

noEnvs :: Text
noEnvs =
  [exon|The flake config contains no managed envs.
Most likely this means that you ran the CLI directly.
Please use one of the flake apps '.#bump', .#lower.init', '.#lower.optimize' or '.#lower.stabilize'.|]

unknownEnv :: EnvName -> Text
unknownEnv name =
  [exon|You requested to update the env '##{name}', but it is not present in the managed deps config.
Maybe this env is not enabled for managed dependencies.|]

selectEnvs :: ManagedEnv -> [EnvName] -> M (NonEmpty (EnvName, EnvConfig))
selectEnvs managedEnv specified = do
  envs <- noteClient noEnvs (nonEmpty specified <|> nonEmpty (Map.keys (ntMap managedEnv.envs)))
  for envs \ env -> do
    conf <- noteClient (unknownEnv env) (managedEnv.envs !! env)
    pure (env, conf)

targetBound :: ManagedOp -> TargetBound
targetBound = \case
  OpBump -> TargetUpper
  OpLowerInit -> TargetLower
  OpLowerOptimize -> TargetLower
  OpLowerStabilize -> TargetLower

-- TODO could the target bound be part of the EnvConfig, so we could run different jobs in one go?
managedJob ::
  ManagedEnv ->
  ManagedOp ->
  EnvName ->
  EnvConfig ->
  M ManagedJob
managedJob env op name envConfig = do
  projectDeps <- depsFromConfig env.packages envConfig.targets
  let targets = sortTargets projectDeps envConfig.targets
      targetDeps = forTargets targets (withManagedRanges env.state.bounds projectDeps)
      remoteDeps = targetRemoteDeps targetDeps
      lowerInit = env.state.lowerInit !! name
      removable = removableBounds (targetBound op) remoteDeps env.state.bounds
      query = uniqueRemoteDeps remoteDeps
  pure ManagedJob {env = name, ..}
  where
    overrides = env.state.overrides !! name

managedJobs ::
  ManagedEnv ->
  ManagedOp ->
  ManagedConfig ->
  M (NonEmpty ManagedJob)
managedJobs env op conf = do
  envs <- selectEnvs env conf.envs
  traverse (uncurry (managedJob env op)) envs

managedApp ::
  BuildHandlers ->
  ManagedEnv ->
  ManagedConfig ->
  ManagedOp ->
  (ManagedApp -> M a) ->
  M a
managedApp build env conf operation use = do
  jobs <- managedJobs env operation conf
  use ManagedApp {
    build,
    conf = conf.stateFile,
    state = env.state,
    packages = env.packages,
    jobs,
    solverBounds = env.lower.solverBounds,
    operation
  }

processAppResult ::
  ReportMutation a =>
  ReportHandlers a ->
  ManagedConfig ->
  ManagedApp ->
  Either [DepMutation a] (BuildResults a) ->
  M ()
processAppResult report conf app = \case
  Right results -> do
    updateProject app.build.stateFile report conf.stateFile results
    let output = buildOutput app.operation results
    Env {output = format, target} <- ask
    outputResult output target format
  Left mutations ->
    reportMutations mutations

runManagedApp ::
  ReportMutation a =>
  BuildHandlers ->
  ReportHandlers a ->
  ManagedEnv ->
  ManagedConfig ->
  ManagedOp ->
  (ManagedApp -> M (Either [DepMutation a] (BuildResults a))) ->
  M ()
runManagedApp build report env conf op use =
  managedApp build env conf op \ app -> do
    result <- use app
    processAppResult report conf app result
