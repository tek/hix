module Hix.Terminal.Geometry where

import Control.Exception (throwIO)
import Control.Monad.Catch (bracket)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import qualified GHC.List as List
import System.Console.ANSI (cursorPosition, hReportCursorPosition, hRestoreCursor, hSaveCursor, hSetCursorPosition)
import System.IO (BufferMode (..), Handle, hFlush, hGetBuffering, hGetChar, hGetEcho, hReady, hSetBuffering, hSetEcho)
import System.IO.Error (userError)
import System.Timeout (timeout)
import Text.ParserCombinators.ReadP (readP_to_S)

import Hix.Maybe (fromMaybeA)

data RenderState =
  RenderState {
    hIn :: Handle,
    hOut :: Handle
  }
  deriving stock (Eq, Show)

type Render m a = ReaderT RenderState m a

runRender :: Handle -> Handle -> Render m a -> m a
runRender hIn hOut ma =
  runReaderT ma RenderState {..}

readWrite ::
  MonadIO m =>
  (Handle -> Handle -> IO a) ->
  Render m a
readWrite f = do
  RenderState {hIn, hOut} <- ask
  liftIO $ f hIn hOut

getReportedCursorPosition :: Handle -> IO String
getReportedCursorPosition h = getReport h "\ESC[" ["R"]

getReport :: Handle -> String -> [String] -> IO String
getReport _ _ [] = error "getReport requires a list of terminating sequences."
getReport handle startChars endChars = do
  fromMaybe "" <$> timeout 500_000 (getStart startChars "")
 where
  endChars' = mapMaybe uncons endChars

  getStart :: String -> String -> IO String
  getStart "" r = getRest r
  getStart (h:hs) r = do
    c <- hGetChar handle
    if c == h
      then getStart hs (c:r)
      else pure $ reverse (c:r)

  getRest :: String -> IO String
  getRest r = do
    c <- hGetChar handle
    case List.lookup c endChars' of
      Nothing -> getRest (c:r)
      Just es -> getEnd es (c:r)

  getEnd :: String -> String -> IO String
  getEnd "" r = pure $ reverse r
  getEnd (e:es) r = do
    c <- hGetChar handle
    if c /= e
      then getRest (c:r)
      else getEnd es (c:r)

hGetCursorPosition ::
  Handle ->
  Handle ->
  IO (Maybe (Int, Int))
hGetCursorPosition hIn hOut = fmap to0base <$> getCursorPosition'
 where
  to0base (row, col) = (row - 1, col - 1)
  getCursorPosition' = do
    input <- bracket (hGetBuffering hIn) (hSetBuffering hIn) $ \_ -> do
      hSetBuffering hIn NoBuffering
      bracket (hGetEcho hIn) (hSetEcho hIn) $ \_ -> do
        hSetEcho hIn False
        clearStdin
        hReportCursorPosition hOut
        hFlush hOut
        getReportedCursorPosition hIn
    case readP_to_S cursorPosition input of
      [] -> pure Nothing
      [((row, col),_)] -> pure $ Just (row, col)
      (_:_) -> pure Nothing
  clearStdin = do
    isReady <- hReady hIn
    when isReady $ do
      _ <- hGetChar hIn
      clearStdin

hGetTerminalSize ::
  Handle ->
  Handle ->
  IO (Maybe (Int, Int))
hGetTerminalSize hIn hOut = do
  hSaveCursor hOut
  hSetCursorPosition hOut 9999999 9999999
  mPos <- hGetCursorPosition hIn hOut
  hRestoreCursor hOut
  hFlush hOut
  pure $ fmap (\(r, c) -> (r + 1, c + 1)) mPos

currentCursor ::
  MonadIO m =>
  Render m (Maybe (Int, Int))
currentCursor = readWrite hGetCursorPosition

-- | Return @((termHeight, termWidth), (cursorRow, cursorCol))@
tryTerminalGeometry ::
  MonadIO m =>
  Render m (Maybe ((Int, Int), (Int, Int)))
tryTerminalGeometry =
  runMaybeT do
    geo <- MaybeT (readWrite hGetTerminalSize)
    cursor <- MaybeT currentCursor
    pure (geo, cursor)

insufficientTerminal :: String
insufficientTerminal =
  "The current terminal does not properly report its size or the cursor position"

terminalGeometry ::
  MonadIO m =>
  Render m ((Int, Int), (Int, Int))
terminalGeometry =
  fromMaybeA (liftIO (throwIO (userError insufficientTerminal))) =<< tryTerminalGeometry
