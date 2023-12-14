module Hix.Managed.Project where

import Hix.Data.Monad (M)
import qualified Hix.Managed.Data.BuildResults
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import qualified Hix.Managed.Handlers.Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.StateFile (writeProjectState)

updateProject ::
  StateFileHandlers ->
  ReportHandlers a ->
  StateFileConfig ->
  BuildResults a ->
  M ()
updateProject stateFile report conf results = do
  writeProjectState stateFile conf results.state
  report.mutations results
