module Hix.Managed.Lower.App where

import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions (LowerOptions))
import Hix.Json (jsonConfigE)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.LowerConfig (LowerConfig, lowerConfig)
import Hix.Managed.Data.ProjectContext (ProjectContext)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Build.Test (chooseHandlers)
import Hix.Managed.Lower.Auto (lowerAutoMain)
import Hix.Managed.Lower.Init (lowerInitMain)
import Hix.Managed.Lower.Optimize (lowerOptimizeMain)
import Hix.Managed.Lower.Stabilize (lowerStabilizeMain)
import Hix.Managed.ProjectContext (withProjectContext)

lowerCli ::
  (LowerConfig -> BuildHandlers -> ProjectContext -> M ProjectResult) ->
  LowerOptions ->
  M ()
lowerCli main opts@LowerOptions {common} = do
  context <- jsonConfigE Client common.context
  handlers <- chooseHandlers common.stateFile context.envs context.buildOutputsPrefix common.project.build.timeout
    common.cabal common.handlers
  withProjectContext handlers common.project context (main conf handlers)
  where
    conf = lowerConfig opts

lowerInitCli :: LowerOptions -> M ()
lowerInitCli = lowerCli lowerInitMain

lowerOptimizeCli :: LowerOptions -> M ()
lowerOptimizeCli = lowerCli (const lowerOptimizeMain)

lowerStabilizeCli :: LowerOptions -> M ()
lowerStabilizeCli = lowerCli (const lowerStabilizeMain)

lowerAutoCli :: LowerOptions -> M ()
lowerAutoCli = lowerCli lowerAutoMain
