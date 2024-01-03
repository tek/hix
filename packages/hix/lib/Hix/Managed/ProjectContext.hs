module Hix.Managed.ProjectContext where

import Control.Monad.Trans.Reader (ask)

import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M (M))
import qualified Hix.Data.Options
import Hix.Data.Options (ProjectOptions)
import Hix.Managed.BuildOutput (buildOutput, outputResult)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto)
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Data.ProjectResult (ProjectResult)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Report
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto
import Hix.Managed.StateFile (writeProjectState)

updateProject ::
  BuildHandlers ->
  BuildConfig ->
  ProjectResult ->
  M ()
updateProject handlers buildConf results = do
  unless buildConf.validate do
    writeProjectState handlers.stateFile results.state
  handlers.report.mutations results

processBuildResults ::
  BuildHandlers ->
  BuildConfig ->
  ProjectResult ->
  M ()
processBuildResults handlers buildConf results = do
  updateProject handlers buildConf results
  AppResources {output = format, target} <- M ask
  outputResult (buildOutput results) target format

withProjectContext ::
  BuildHandlers ->
  ProjectOptions ->
  ProjectContextProto ->
  (ProjectContext -> M ProjectResult) ->
  M ()
withProjectContext handlers opts proto use = do
  project <- ProjectContextProto.validate opts proto
  result <- use project
  processBuildResults handlers opts.build result
