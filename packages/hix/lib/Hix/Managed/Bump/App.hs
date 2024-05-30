module Hix.Managed.Bump.App where

import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (BumpOptions (BumpOptions))
import Hix.Json (jsonConfigE)
import Hix.Managed.Bump.Optimize (bumpOptimizeMain)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Handlers.Build.Test (chooseHandlers)
import Hix.Managed.ProjectContext (withProjectContext)

bumpCli :: BumpOptions -> M ()
bumpCli BumpOptions {common = opts} = do
  context <- jsonConfigE Client opts.context
  handlers <- liftIO $
    chooseHandlers opts.stateFile context.envs context.buildOutputsPrefix opts.project.build opts.cabal opts.handlers
  withProjectContext handlers opts.project context (bumpOptimizeMain handlers)
