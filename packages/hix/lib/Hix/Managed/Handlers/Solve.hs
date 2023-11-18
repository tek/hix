module Hix.Managed.Handlers.Solve where

import Hix.Data.Bounds (Bounds)
import Hix.Data.Version (NewVersion)
import Hix.Managed.Solve.Changes (SolverPlan)
import Hix.Data.Monad (M)

data SolveHandlers =
  SolveHandlers {
    solveForVersion :: Bounds -> NewVersion -> M (Maybe SolverPlan)
  }

handlersNull :: SolveHandlers
handlersNull =
  SolveHandlers {
    solveForVersion = \ _ _ -> pure Nothing
  }
