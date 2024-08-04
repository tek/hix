module Hix.Managed.Handlers.Project where

import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import qualified Hix.Managed.Handlers.StateFile as StateFile
import Hix.Managed.Handlers.StateFile (StateFileHandlers)

data ProjectHandlers =
  ProjectHandlers {
    stateFile :: StateFileHandlers,
    report :: ReportHandlers
  }

handlersNull :: ProjectHandlers
handlersNull =
  ProjectHandlers {
    stateFile = StateFile.handlersNull,
    report = Report.handlersNull
  }
