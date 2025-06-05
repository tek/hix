module Hix.Terminal.State where

import Control.Monad.Catch (bracket)
import Data.IORef (IORef, newIORef, readIORef)
import System.Console.ANSI (hHideCursor, hSaveCursor, hSetCursorColumn, hShowCursor)
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hSetBuffering, hSetEcho, stdin)
import System.Posix (Fd)

import Hix.Data.Monad (M)
import Hix.Monad (appContextVerboseIO)
import Hix.Terminal.Geometry (runRender, terminalGeometry)

data VtyTuiResources =
  VtyTuiResources {
    fdIn :: Fd,
    fdOut :: Fd,
    hIn :: Handle,
    hOut :: Handle
  }
  deriving stock (Generic)

data OutputState =
  OutputState {
    geometry :: (Int, Int),
    renderHeight :: Maybe Int,
    firstLine :: Int
  }
  deriving stock (Eq, Show, Generic)

fromState ::
  IORef OutputState ->
  (OutputState -> a) ->
  IO a
fromState ref f =
  f <$> readIORef ref

hInitializeTerminal ::
  Handle ->
  Handle ->
  IO BufferMode
hInitializeTerminal hIn hOut = do
  hSetCursorColumn hOut 0
  hSaveCursor hOut
  hHideCursor hOut
  hFlush hOut
  hSetEcho hIn False
  hGetBuffering stdin <* hSetBuffering hIn NoBuffering

hResetTerminal ::
  Handle ->
  Handle ->
  BufferMode ->
  IO ()
hResetTerminal hIn hOut hInMode = do
  hShowCursor hOut
  hFlush hOut
  hSetEcho hIn True
  hSetBuffering hIn hInMode

newOutputState ::
  IORef VtyTuiResources ->
  IO (IORef OutputState)
newOutputState var = do
  VtyTuiResources {hIn, hOut} <- readIORef var
  ((height, width), (firstLine, _)) <- runRender hIn hOut terminalGeometry
  newIORef OutputState {geometry = (width, height), firstLine, renderHeight = Nothing}

bracketTerminal ::
  IORef VtyTuiResources ->
  (IORef OutputState -> IO a) ->
  M a
bracketTerminal tuiState use = do
  appContextVerboseIO "running brick app" do
    VtyTuiResources {hIn, hOut} <- readIORef tuiState
    bracket (hInitializeTerminal hIn hOut) (hResetTerminal hIn hOut) \ _ -> do
      state <- newOutputState tuiState
      use state
