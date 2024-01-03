module Hix.Managed.Lower.App where

import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions (LowerOptions))
import Hix.Json (jsonConfigE)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.LowerConfig (LowerConfig, lowerConfig)
import Hix.Managed.Data.ProjectContext (ProjectContext)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildOutputsPrefix)
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers, SpecialLowerHandlers (TestLowerHandlers))
import qualified Hix.Managed.Handlers.Lower.Prod as Lower
import qualified Hix.Managed.Handlers.Lower.Test as Lower
import Hix.Managed.Lower.Auto (lowerAutoMain)
import Hix.Managed.Lower.Init (lowerInitMain)
import Hix.Managed.Lower.Optimize (lowerOptimizeMain)
import Hix.Managed.Lower.Stabilize (lowerStabilizeMain)
import Hix.Managed.ProjectContext (withProjectContext)

chooseHandlers ::
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  Maybe SpecialLowerHandlers ->
  M LowerHandlers
chooseHandlers stateFileConf envsConf buildOutputsPrefix = \case
  Just TestLowerHandlers -> Lower.handlersTest stateFileConf envsConf buildOutputsPrefix oldest
  Nothing -> Lower.handlersProd stateFileConf envsConf buildOutputsPrefix oldest
  where
    oldest = False

lowerCli ::
  (LowerConfig -> LowerHandlers -> ProjectContext -> M ProjectResult) ->
  LowerOptions ->
  M ()
lowerCli main opts@LowerOptions {common, handlers = specialHandlers} = do
  context <- jsonConfigE Client common.context
  handlers <- chooseHandlers common.stateFile context.envs context.buildOutputsPrefix specialHandlers
  withProjectContext handlers.build common.project context (main conf handlers)
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
