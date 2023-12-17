module Hix.Managed.Lower.Stabilize where

import Hix.Class.Map (ntTo, (!!))
import qualified Hix.Data.Dep
import Hix.Data.LowerConfig (LowerConfig)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState (ManagedState))
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Managed.Data.BuildResult (BuildResult (FatalBuildFailure, NoActionRequired))
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.BuildState (BuildStatus (Failure, Success))
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedJob
import qualified Hix.Managed.Data.ManagedJob as ManagedJob
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
import Hix.Managed.Lower.Data.Lower (Lower)
import Hix.Managed.Lower.Data.LowerMode (lowerStabilizeMode)
import Hix.Managed.Overrides (newVersionOverrides)

lowerInitState ::
  ManagedApp ->
  ManagedJob ->
  ManagedState ->
  M ManagedState
lowerInitState app job state = do
  overrides <- newVersionOverrides app.build.hackage newVersions
  pure ManagedState {bounds = state.bounds, overrides}
  where
    newVersions = ntTo job.lowerInit \ name version -> PackageId {..}

lowerStabilizeEnv ::
  LowerHandlers ->
  LowerConfig ->
  ManagedApp ->
  Builder ->
  ManagedJob ->
  M (BuildResult Lower)
lowerStabilizeEnv handlers conf app builder job =
  buildJob builder job managed0 >>= \case
    Success -> pure NoActionRequired
    Failure -> stabilize
  where
    stabilize = do
      state <- lowerInitState app job managed0
      buildJob builder job state >>= \case
        Success ->
          lowerJob upperBounds (candidates job.lowerInit) mutationHandlers app conf builder job
        Failure ->
          pure (FatalBuildFailure initLowerFailed)

    managed0 = ManagedJob.initialState job
    candidates initialVersions dep = candidatesStabilize handlers.versions dep (initialVersions !! dep.package)
    mutationHandlers = Mutation.handlersLower conf lowerStabilizeMode (handlers.solve app.packages)

    -- TODO Be helpful about what to do
    initLowerFailed = "Build with initial lower bounds failed."

lowerStabilize ::
  LowerHandlers ->
  LowerConfig ->
  ManagedApp ->
  M (BuildResults Lower)
lowerStabilize handlers conf app =
  buildJobs app (lowerStabilizeEnv handlers conf app)
