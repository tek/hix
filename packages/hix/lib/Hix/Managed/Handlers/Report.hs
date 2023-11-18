module Hix.Managed.Handlers.Report where

import Hix.Data.Bounds (UninitializedBounds)
import Hix.Managed.Data.Build (BuildResult)
import Hix.Monad (M)

data ReportHandlers a =
  ReportHandlers {
    mutations :: UninitializedBounds -> BuildResult a -> M ()
  }

handlersNull :: ReportHandlers a
handlersNull =
  ReportHandlers {
    mutations = \ _ _ -> unit
  }
