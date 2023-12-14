module Hix.Managed.Lower.Init where

import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig, LowerInitConfig (LowerInitConfig))
import Hix.Data.Monad (M)
import Hix.Managed.Data.BuildResult (BuildResult)
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.ManagedApp (ManagedApp)
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpLowerInit))
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
import Hix.Managed.Data.SolverParams (keepBounds)
import Hix.Managed.Handlers.Build (Builder)
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers)
import qualified Hix.Managed.Handlers.Mutation.Lower as Mutation
import Hix.Managed.Job (buildJobs)
import Hix.Managed.Lower.Build (lowerJob)
import Hix.Managed.Lower.Candidates (candidatesInit)
import Hix.Managed.Lower.Data.Lower (Lower)

lowerInitJob ::
  LowerHandlers ->
  LowerConfig ->
  LowerInitConfig ->
  ManagedApp ->
  Builder ->
  ManagedJob ->
  M (BuildResult Lower)
lowerInitJob handlers lowerConf LowerInitConfig {reset} app builder job =
  lowerJob initialBounds candidates mutationHandlers app lowerConf builder job
  where
    initialBounds deps = lowerConf.initialSolverParams <> keepBounds keep deps
    candidates = candidatesInit handlers.versions keep
    mutationHandlers = Mutation.handlersLower OpLowerInit lowerConf handlers.solve
    keep | reset = mempty
         | otherwise = job.lowerInit

lowerInit ::
  LowerHandlers ->
  LowerConfig ->
  LowerInitConfig ->
  ManagedApp ->
  M (BuildResults Lower)
lowerInit handlers lowerConf conf app =
  buildJobs app (lowerInitJob handlers lowerConf conf app)
