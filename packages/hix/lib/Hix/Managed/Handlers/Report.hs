module Hix.Managed.Handlers.Report where

import Hix.Data.Monad (M)
import Hix.Managed.Data.BuildResults (BuildResults)

data ReportHandlers a =
  ReportHandlers {
    mutations :: BuildResults a -> M ()
  }

handlersNull :: ReportHandlers a
handlersNull =
  ReportHandlers {
    mutations = \ _ -> unit
  }
