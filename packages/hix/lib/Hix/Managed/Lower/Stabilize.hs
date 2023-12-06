module Hix.Managed.Lower.Stabilize where

import Exon (exon)

import Hix.Class.Map (ntTo, (!!))
import qualified Hix.Data.Dep
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState (ManagedState))
import Hix.Data.Monad (M)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import Hix.Managed.Data.Build (BuildResult, BuildResults, BuildStatus (Failure, Success), emptyBuildResult, fatal)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpLowerStabilize))
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import Hix.Managed.Data.SolverParams (upperBounds)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (Builder)
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers)
import qualified Hix.Managed.Handlers.Mutation.Lower as Mutation
import Hix.Managed.Job (buildJob, buildJobs)
import Hix.Managed.Lower.Build (lowerJob)
import Hix.Managed.Lower.Candidates (candidatesStabilize)
import Hix.Managed.Lower.Data.Lower (Lower, LowerState (LowerState))
import Hix.Managed.Overrides (newVersionOverrides)

lowerInitState ::
  ManagedApp ->
  ManagedJob ->
  M ManagedState
lowerInitState app job = do
  overrides <- newVersionOverrides app.build.hackage newVersions
  pure ManagedState {bounds = job.state.bounds, overrides}
  where
    newVersions = ntTo job.lowerInit \ package version -> NewVersion {..}

lowerStabilizeEnv ::
  LowerHandlers ->
  LowerConfig ->
  ManagedApp ->
  Builder ->
  ManagedJob ->
  M (BuildResult Lower)
lowerStabilizeEnv handlers conf app builder job =
  buildJob builder job job.state >>= \case
    Success -> pure initResult
    Failure -> stabilize
  where
    stabilize = do
      initialState <- lowerInitState app job
      buildJob builder job initialState >>= \case
        Success ->
          lowerJob initialBounds LowerState (candidates job.lowerInit) mutationHandlers app builder job
        Failure ->
          pure initResult {fatal = Just initLowerFailed}

    initialBounds deps = conf.initialSolverParams <> upperBounds deps
    candidates initialVersions dep = candidatesStabilize handlers.versions dep (initialVersions !! dep.package)
    mutationHandlers = Mutation.handlersLower OpLowerStabilize conf handlers.solve

    initResult = emptyBuildResult job.state

    -- TODO Be helpful about what to do
    initLowerFailed = [exon|Build with initial lower bounds for '##{env}' failed.|]

    env = job.env

lowerStabilize ::
  LowerHandlers ->
  LowerConfig ->
  ManagedApp ->
  M (BuildResults Lower)
lowerStabilize handlers conf app =
  buildJobs app (lowerStabilizeEnv handlers conf app)
