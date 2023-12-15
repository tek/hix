module Hix.Managed.Handlers.Build.Test where

import Data.IORef (IORef)

import Hix.Data.ManagedEnv (BuildOutputsPrefix)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Data.Version (Versions)
import Hix.Managed.Data.BuildState (BuildStatus)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildHandlers (..), handlersNull, versionsBuilder)
import Hix.Managed.Handlers.Build.Prod (handlersProd)
import qualified Hix.Managed.Handlers.StateFile.Test as StateFileHandlers

handlersUnitTest ::
  (Versions -> M BuildStatus) ->
  IO (BuildHandlers, IORef [Expr])
handlersUnitTest builder = do
  (stateFile, stateFileRef) <- StateFileHandlers.handlersUnitTest
  pure (handlersNull {stateFile, withBuilder = versionsBuilder builder}, stateFileRef)

handlersTest ::
  StateFileConfig ->
  Maybe BuildOutputsPrefix ->
  IO BuildHandlers
handlersTest = handlersProd
