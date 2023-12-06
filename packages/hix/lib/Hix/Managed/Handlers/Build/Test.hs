module Hix.Managed.Handlers.Build.Test where

import Data.IORef (IORef)

import Hix.Data.ManagedEnv (BuildOutputsPrefix)
import Hix.Data.NixExpr (Expr)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildHandlers (..), handlersNull)
import Hix.Managed.Handlers.Build.Prod (handlersProd)
import qualified Hix.Managed.Handlers.StateFile.Test as StateFileHandlers

handlersUnitTest ::  IO (BuildHandlers, IORef [Expr])
handlersUnitTest = do
  (stateFile, stateFileRef) <- StateFileHandlers.handlersUnitTest
  pure (handlersNull {stateFile}, stateFileRef)

handlersTest ::
  StateFileConfig ->
  Maybe BuildOutputsPrefix ->
  IO BuildHandlers
handlersTest = handlersProd
