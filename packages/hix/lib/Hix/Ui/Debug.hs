module Hix.Ui.Debug where

import Brick (App (..), BrickEvent (..), EventM)
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async.Lifted (wait, withAsync)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Exon (exon)
import Graphics.Vty (Event (..), Key (..))
import System.Timeout (timeout)
import qualified Text.Show

import qualified Hix.Data.Error as Error
import Hix.Data.Error (Error)
import Hix.Data.Monad (AppResources (..))
import Hix.Monad (throwME)

data BrickDebugEvent =
  BrickVtyEvent Event
  |
  BrickStarted
  deriving stock (Eq, Show)

data BrickDebug =
  BrickDebug {
     input :: BChan BrickDebugEvent,
     output :: Chan BrickDebugEvent
  }

instance Show BrickDebug where
  show _ = "BrickDebug"

brickDebug :: MonadIO m => m BrickDebug
brickDebug = do
  input <- liftIO $ newBChan 4
  output <- liftIO $ newChan
  pure BrickDebug {..}

sendBrickDebugEvent ::
  MonadIO m =>
  MonadReader AppResources m =>
  MonadError Error m =>
  BrickDebug ->
  BrickDebugEvent ->
  m ()
sendBrickDebugEvent BrickDebug {input, output} event = do
  result <- liftIO do
    writeBChan input event
    timeout 3_000_000 (readChan output)
  case result of
    Just _ -> unit
    Nothing -> throwME (Error.Client "Event completion signal was not received within 3 seconds")

sendBrick ::
  MonadIO m =>
  MonadReader AppResources m =>
  MonadError Error m =>
  BrickDebug ->
  Event ->
  m ()
sendBrick debug event =
  sendBrickDebugEvent debug (BrickVtyEvent event)

sendKey ::
  MonadIO m =>
  MonadReader AppResources m =>
  MonadError Error m =>
  BrickDebug ->
  Key ->
  m ()
sendKey debug key =
  sendBrick debug (EvKey key [])

sendChar ::
  MonadIO m =>
  MonadReader AppResources m =>
  MonadError Error m =>
  BrickDebug ->
  Char ->
  m ()
sendChar debug c =
  sendKey debug (KChar c)

withDebugSignal ::
  BrickDebug ->
  (BrickEvent n BrickDebugEvent -> EventM n s ()) ->
  BrickEvent n BrickDebugEvent ->
  EventM n s ()
withDebugSignal BrickDebug {output} original event = do
  original event
  sendSignal
  where
    sendSignal = case event of
      AppEvent e -> liftIO $ writeChan output e
      _ -> unit

waitForApp ::
  MonadIO m =>
  MonadReader AppResources m =>
  MonadError Error m =>
  BrickDebug ->
  m ()
waitForApp BrickDebug {output} =
  liftIO (timeout 3_000_000 (readChan output)) >>= \case
    Just BrickStarted -> unit
    Just e -> throwME (Error.Client [exon|First event from Brick app is not BrickStarted: #{show e}|])
    _ -> throwME (Error.Client "No start event received from Brick app before timeout")

brickDebugAsync ::
  MonadIO m =>
  MonadError Error m =>
  MonadReader AppResources m =>
  MonadBaseControl IO m =>
  BrickDebug ->
  m a ->
  (m () -> m ()) ->
  m a
brickDebugAsync debug prog test = do
  withAsync prog \ asyncResult -> do
    test (waitForApp debug)
    wait asyncResult

brickAppDebug ::
  BrickDebug ->
  App s BrickDebugEvent n ->
  App s BrickDebugEvent n
brickAppDebug debug app =
  app {
    appHandleEvent = withDebugSignal debug app.appHandleEvent,
    appStartEvent = withDebugSignal debug (const app.appStartEvent) (AppEvent BrickStarted)
  }
