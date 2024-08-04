module Hix.Managed.Lower.App where

import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions)
import Hix.Managed.App (managedApp)
import Hix.Managed.Data.LowerConfig (LowerConfig, lowerConfig)
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Context (ContextKey (ContextManaged), jsonOrQueryProd)
import Hix.Managed.Lower.Auto (lowerAutoMain)
import Hix.Managed.Lower.Init (lowerInitMain)
import Hix.Managed.Lower.Optimize (lowerOptimizeMain)
import Hix.Managed.Lower.Stabilize (lowerStabilizeMain)
import Hix.Managed.ProjectContext (processProjectResult)

lowerApp ::
  (LowerConfig -> BuildHandlers -> ProjectContext -> M ProjectResult) ->
  LowerOptions ->
  M ProjectResult
lowerApp main opts = do
  context <- jsonOrQueryProd ContextManaged opts.common.context
  managedApp opts.common context (main (lowerConfig opts))

lowerCli ::
  (LowerConfig -> BuildHandlers -> ProjectContext -> M ProjectResult) ->
  LowerOptions ->
  M ()
lowerCli main opts =
  processProjectResult =<< lowerApp main opts

lowerInitCli :: LowerOptions -> M ()
lowerInitCli = lowerCli lowerInitMain

lowerOptimizeCli :: LowerOptions -> M ()
lowerOptimizeCli = lowerCli (const lowerOptimizeMain)

lowerStabilizeCli :: LowerOptions -> M ()
lowerStabilizeCli = lowerCli (const lowerStabilizeMain)

lowerAutoCli :: LowerOptions -> M ()
lowerAutoCli = lowerCli lowerAutoMain
