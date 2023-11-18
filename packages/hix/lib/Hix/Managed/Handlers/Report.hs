module Hix.Managed.Handlers.Report where

import Hix.Data.Bounds (RemovableBounds)
import Hix.Managed.Data.Build (BuildResult)
import Hix.Data.Monad (M)

data ReportHandlers a =
  ReportHandlers {
    mutations :: RemovableBounds -> BuildResult a -> M ()
  }

handlersNull :: ReportHandlers a
handlersNull =
  ReportHandlers {
    mutations = \ _ _ -> unit
  }
