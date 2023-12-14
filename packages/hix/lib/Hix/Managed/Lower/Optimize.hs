module Hix.Managed.Lower.Optimize where

import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig)
import Hix.Data.Monad (M)
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.ManagedApp (ManagedApp)
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpLowerOptimize))
import Hix.Managed.Data.SolverParams (lowerBounds)
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers)
import qualified Hix.Managed.Handlers.Mutation.Lower as Mutation
import Hix.Managed.Job (buildJobs)
import Hix.Managed.Lower.Build (lowerJob)
import Hix.Managed.Lower.Candidates (candidatesOptimize)
import Hix.Managed.Lower.Data.Lower (Lower)

lowerOptimize ::
  LowerHandlers ->
  LowerConfig ->
  ManagedApp ->
  M (BuildResults Lower)
lowerOptimize handlers conf app =
  buildJobs app (lowerJob initialBounds candidates mutationHandlers app conf)
  where
    initialBounds deps = conf.initialSolverParams <> lowerBounds deps
    candidates = candidatesOptimize handlers.versions
    mutationHandlers = Mutation.handlersLower OpLowerOptimize conf handlers.solve
