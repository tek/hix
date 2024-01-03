module Hix.Managed.Handlers.Report where

import Hix.Data.Monad (M)
import Hix.Managed.Data.ProjectResult (ProjectResult)

data ReportHandlers =
  ReportHandlers {
    mutations :: ProjectResult -> M ()
  }

handlersNull :: ReportHandlers
handlersNull =
  ReportHandlers {mutations = \ _ -> unit}
