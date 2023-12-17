module Hix.Managed.Job where

import Control.Monad (foldM)
import Exon (exon)

import Hix.Data.EnvName (EnvName)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Log as Log
import Hix.Managed.Data.BuildResult (BuildResult)
import Hix.Managed.Data.BuildResults (BuildResults, initBuildResults, updateBuildResults)
import Hix.Managed.Data.BuildState (BuildStatus)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers (BuildHandlers), Builder (Builder), EnvBuilder (EnvBuilder))

withJobBuilder :: Builder -> ManagedJob -> ManagedState -> (EnvBuilder -> M a) -> M a
withJobBuilder Builder {withEnvBuilder} job =
  withEnvBuilder job.env job.targets job.remoteDeps

buildJob :: Builder -> ManagedJob -> ManagedState -> M BuildStatus
buildJob builder job state =
  withJobBuilder builder job state \ EnvBuilder {buildWithState} -> buildWithState state

buildJobs ::
  ManagedApp ->
  (Builder -> ManagedJob -> M (BuildResult a)) ->
  M (BuildResults a)
buildJobs app use =
  withBuilder \ builder ->
    foldM (build builder) (initBuildResults app.state) app.jobs
  where
    build builder results job = do
      Log.debug [exon|Processing env '##{job.env :: EnvName}'|]
      res <- use builder job
      pure (updateBuildResults app.operation job.env job.remoteDeps results res)

    BuildHandlers {withBuilder} = app.build
