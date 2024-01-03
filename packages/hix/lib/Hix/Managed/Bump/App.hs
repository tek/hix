module Hix.Managed.Bump.App where

import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (BumpOptions (BumpOptions))
import Hix.Json (jsonConfigE)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildOutputsPrefix)
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers, SpecialBumpHandlers (TestBumpHandlers))
import Hix.Managed.Handlers.Bump.Prod (handlersProd)
import Hix.Managed.Handlers.Bump.Test (handlersTest)
import Hix.Managed.ProjectContext (withProjectContext)

chooseHandlers ::
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  Maybe SpecialBumpHandlers ->
  IO (BumpHandlers)
chooseHandlers stateFileConf envsConf buildOutputsPrefix = \case
  Just TestBumpHandlers -> handlersTest stateFileConf envsConf buildOutputsPrefix False
  Nothing -> handlersProd stateFileConf envsConf buildOutputsPrefix False

bumpCli :: BumpOptions -> M ()
bumpCli BumpOptions {common = opts, handlers = specialHandlers} = do
  context <- jsonConfigE Client opts.context
  handlers <- liftIO (chooseHandlers opts.stateFile context.envs context.buildOutputsPrefix specialHandlers)
  withProjectContext handlers.build opts.project context (bumpOptimizeMain handlers)
