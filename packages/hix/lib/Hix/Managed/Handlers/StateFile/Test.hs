module Hix.Managed.Handlers.StateFile.Test where

import Data.IORef (IORef, modifyIORef, newIORef)
import Path (Abs, File, Path)

import Hix.Managed.Handlers.StateFile (StateFileHandlers, handlersNull, writeFile)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)

writeFileIORef ::
  IORef [Expr] ->
  Path Abs File ->
  Expr ->
  M ()
writeFileIORef out _ expr =
  liftIO $ modifyIORef out (expr :)

handlersUnitTest :: IO (StateFileHandlers, IORef [Expr])
handlersUnitTest = do
  ref <- newIORef []
  let handlers = handlersNull {
    writeFile = writeFileIORef ref
  }
  pure (handlers, ref)
