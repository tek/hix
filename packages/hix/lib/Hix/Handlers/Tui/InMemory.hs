module Hix.Handlers.Tui.InMemory where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)

import Hix.Data.Monad (LogLevel, M)
import Hix.Handlers.Tui (TuiHandlers (..))

logIORef :: IORef [Text] -> LogLevel -> Text -> M ()
logIORef ref _ msg =
  liftIO (modifyIORef' ref (msg :))

handlersInMemory ::
  IORef [Text] ->
  TuiHandlers M ->
  TuiHandlers M
handlersInMemory ref base =
  base {log = logIORef ref}

withTuiInMemory ::
  TuiHandlers M ->
  (TuiHandlers M -> IO a) ->
  IO ([Text], a)
withTuiInMemory base use = do
  logRef <- newIORef []
  result <- use (handlersInMemory logRef base)
  log <- readIORef logRef
  pure (log, result)
