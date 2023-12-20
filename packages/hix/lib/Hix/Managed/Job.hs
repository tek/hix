module Hix.Managed.Job where

import Control.Monad (foldM)
import Exon (exon)

import Hix.Data.EnvName (EnvName)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Log as Log
import qualified Hix.Managed.Data.BuildDomain
import Hix.Managed.Data.BuildDomain (BuildDomain)
import Hix.Managed.Data.BuildResult (BuildResult)
import Hix.Managed.Data.BuildResults (BuildResults, initBuildResults, updateBuildResults)
import Hix.Managed.Data.BuildState (BuildStatus)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers (BuildHandlers), Builder (Builder), EnvBuilder (EnvBuilder))

buildState :: Builder -> BuildDomain -> ManagedState -> M BuildStatus
buildState Builder {withEnvBuilder} domain state =
  withEnvBuilder domain state \ EnvBuilder {buildWithState} -> buildWithState state

buildJob :: Builder -> ManagedJob -> ManagedState -> M BuildStatus
buildJob builder job = buildState builder job.domain

buildJobs ::
  ManagedApp ->
  (Builder -> ManagedJob -> M (BuildResult a)) ->
  M (BuildResults a)
buildJobs app use =
  withBuilder \ builder ->
    foldM (build builder) (initBuildResults app.state) app.jobs
  where
    build builder results job = do
      Log.debug [exon|Processing env '##{job.domain.env :: EnvName}'|]
      res <- use builder job
      pure (updateBuildResults app.operation job.domain.env job.domain.deps results res)

    BuildHandlers {withBuilder} = app.build
