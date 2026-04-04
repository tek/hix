module Hix.Terminal.State where

import Control.Monad.Catch (bracket)
import Data.IORef (IORef, newIORef, readIORef)
import System.Console.ANSI (
  hClearLine,
  hCursorDown,
  hHideCursor,
  hSaveCursor,
  hSetCursorColumn,
  hSetCursorPosition,
  hShowCursor,
  )
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hPutStr, hSetBuffering, hSetEcho, stdin)
import System.Posix (Fd)

import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Error (ignoringIOErrors)
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
    lastRenderHeight :: Int,
    firstLine :: Int
  }
  deriving stock (Eq, Show, Generic)

fromState ::
  IORef OutputState ->
  (OutputState -> a) ->
  IO a
fromState ref f =
  f <$> readIORef ref

newOutputState ::
  IORef VtyTuiResources ->
  IO (IORef OutputState)
newOutputState var = do
  VtyTuiResources {hIn, hOut} <- readIORef var
  ((height, width), (firstLine, _)) <- runRender hIn hOut terminalGeometry
  newIORef OutputState {geometry = (width, height), firstLine, renderHeight = Nothing, lastRenderHeight = 0}

hInitializeTerminal ::
  IORef VtyTuiResources ->
  IO (BufferMode, IORef OutputState)
hInitializeTerminal tuiState = do
  VtyTuiResources {hIn, hOut} <- readIORef tuiState
  hSetCursorColumn hOut 0
  hSaveCursor hOut
  hHideCursor hOut
  hFlush hOut
  hSetEcho hIn False
  bufMode <- hGetBuffering stdin <* hSetBuffering hIn NoBuffering
  stateRef <- newOutputState tuiState
  pure (bufMode, stateRef)

-- | After a Brick app exits, reposition the cursor.
--
-- When @persistent@ is 'True', move below the rendered area so subsequent output doesn't
-- overwrite the previous content.
--
-- When @persistent@ is 'False', move back to the first line of the rendered area and clear
-- the lines so the next output starts where the UI was.
repositionCursor :: Bool -> Handle -> IORef OutputState -> IO ()
repositionCursor persistent hOut stateRef = do
  OutputState {firstLine, lastRenderHeight} <- readIORef stateRef
  when (lastRenderHeight > 0) do
    if persistent
    then do
      -- Move to the last rendered line and emit a newline.
      -- This works even when the cursor is at the bottom of the terminal,
      -- where hSetCursorPosition would be clamped and fail to advance.
      let lastLine = firstLine + lastRenderHeight - 1
      hSetCursorPosition hOut lastLine 0
      hPutStr hOut "\n"
      hFlush hOut
    else do
      -- Move back to the first line and clear the rendered area.
      hSetCursorPosition hOut firstLine 0
      replicateM_ lastRenderHeight do
        hClearLine hOut
        hCursorDown hOut 1
      hSetCursorPosition hOut firstLine 0
      hFlush hOut

hResetTerminal ::
  Bool ->
  IORef VtyTuiResources ->
  (BufferMode, IORef OutputState) ->
  IO ()
hResetTerminal persistent tuiState (hInMode, stateRef) = do
  VtyTuiResources {hIn, hOut} <- readIORef tuiState
  traverse_ @[] ignoringIOErrors [
    repositionCursor persistent hOut stateRef,
    hShowCursor hOut,
    hFlush hOut,
    hSetEcho hIn True,
    hSetBuffering hIn hInMode
    ]

bracketTerminal ::
  IORef VtyTuiResources ->
  (IORef OutputState -> IO a) ->
  M a
bracketTerminal tuiState use = do
  persistent <- appRes.persistentUi
  appContextVerboseIO "running brick app" do
    bracket (hInitializeTerminal tuiState) (hResetTerminal persistent tuiState) \ (_, stateRef) ->
      use stateRef
