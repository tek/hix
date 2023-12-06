module Hix.Managed.Handlers.Solve where

import Hix.Data.Monad (M)
import Hix.Data.Version (NewVersion)
import Hix.Managed.Data.ManagedConfig (ManagedOp)
import Hix.Managed.Data.SolverParams (SolverParams)
import Hix.Managed.Solve.Changes (SolverPlan)

data SolveHandlers =
  SolveHandlers {
    solveForVersion :: ManagedOp -> SolverParams -> NewVersion -> M (Maybe SolverPlan)
  }

handlersNull :: SolveHandlers
handlersNull =
  SolveHandlers {
    solveForVersion = \ _ _ _ -> pure Nothing
  }
