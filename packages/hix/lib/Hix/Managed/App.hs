module Hix.Managed.App where

import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (ManagedOptions)
import Hix.Managed.Data.ProjectContext (ProjectContext (..))
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (..))
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Build.Test (chooseHandlers)
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.ProjectContext (withProjectContext)

managedApp ::
  ManagedOptions ->
  ProjectContextProto ->
  (BuildHandlers -> ProjectContext -> M ProjectResult) ->
  M ProjectResult
managedApp opts proto use = do
  handlersProject <- Project.handlersProd opts.stateFile
  withProjectContext handlersProject opts.project proto \ context -> do
    handlers <- chooseHandlers opts.handlers handlersProject proto.envs context.build context.cabal
    use handlers context
