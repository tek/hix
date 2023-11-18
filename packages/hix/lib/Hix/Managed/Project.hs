module Hix.Managed.Project where

import Hix.Data.Bounds (RemovableBounds)
import Hix.Data.ManagedEnv (ManagedEnvState)
import Hix.Data.Monad (M)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (BuildResult)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Data.ManagedJob (ManagedJob)
import qualified Hix.Managed.Handlers.Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.StateFile (writeProjectState)

updateProject ::
  StateFileHandlers ->
  ReportHandlers a ->
  StateFileConfig ->
  ManagedJob ->
  RemovableBounds ->
  ManagedEnvState ->
  BuildResult a ->
  M ()
updateProject stateFile report conf job removable originalManaged result = do
  writeProjectState stateFile job conf originalManaged result.managed
  report.mutations removable result
