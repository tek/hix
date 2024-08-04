module Hix.Managed.ProjectContext where

import qualified Hix.Data.Monad
import Hix.Data.Monad (AppResources (AppResources), M)
import qualified Hix.Data.Options
import Hix.Data.Options (ProjectOptions)
import Hix.Managed.BuildOutput (buildOutput, depChanges, outputResult)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto)
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import qualified Hix.Managed.Handlers.Report
import qualified Hix.Managed.ProjectContextProto as ProjectContextProto
import Hix.Managed.StateFile (writeProjectState)
import Hix.Monad (ask)

updateProject ::
  ProjectHandlers ->
  Bool ->
  ProjectResult ->
  M ()
updateProject handlers validate result = do
  unless validate do
    writeProjectState handlers.stateFile result.state
  handlers.report.mutations result

withProjectContext ::
  ProjectHandlers ->
  ProjectOptions ->
  ProjectContextProto ->
  (ProjectContext -> M ProjectResult) ->
  M ProjectResult
withProjectContext handlers opts proto use = do
  project <- ProjectContextProto.validate opts proto
  result <- use project
  updateProject handlers opts.build.validate result
  pure result

processProjectResult :: ProjectResult -> M ()
processProjectResult result = do
  AppResources {output = format, target} <- ask
  outputResult target format (buildOutput (depChanges result))
