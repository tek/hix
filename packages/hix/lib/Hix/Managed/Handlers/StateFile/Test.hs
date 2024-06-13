module Hix.Managed.Handlers.StateFile.Test where

import Data.IORef (IORef, modifyIORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Managed.Handlers.StateFile (StateFileHandlers (StateFileHandlers, writeFile))

writeFileIORef ::
  IORef [Expr] ->
  Maybe a ->
  Expr ->
  M ()
writeFileIORef out _ expr =
  liftIO $ modifyIORef out (expr :)

handlersUnitTest ::
  MonadIO m =>
  m (StateFileHandlers, IORef [Expr])
handlersUnitTest = do
  ref <- liftIO (newIORef [])
  let handlers = StateFileHandlers {writeFile = writeFileIORef ref}
  pure (handlers, ref)
