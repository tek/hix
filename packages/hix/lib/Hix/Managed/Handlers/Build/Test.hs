module Hix.Managed.Handlers.Build.Test where

import Data.IORef (IORef)
import Path (Abs, Dir, Path)

import Hix.Managed.Handlers.Build (BuildHandlers (..), TempProjectBracket (TempProjectBracket), handlersNull)
import Hix.Managed.Handlers.Build.Prod (handlersProd)
import qualified Hix.Managed.Handlers.StateFile.Test as StateFileHandlers
import Hix.Monad (M)
import Hix.NixExpr (Expr)

withTempProjectAt :: Path Abs Dir -> TempProjectBracket
withTempProjectAt tmpRoot = TempProjectBracket \ _ use -> use tmpRoot

handlersUnitTest :: Path Abs Dir -> IO (BuildHandlers, IORef [Expr])
handlersUnitTest tmpRoot = do
  (stateFile, stateFileRef) <- StateFileHandlers.handlersUnitTest
  pure (handlersNull {stateFile, withTempProject = withTempProjectAt tmpRoot}, stateFileRef)

handlersTest ::
  Maybe (Path Abs Dir) ->
  M BuildHandlers
handlersTest =
  handlersProd id
