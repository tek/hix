module Hix.Handlers.Tui.Test where

import Brick (App (..), BrickEvent (..), EventM, Widget, attrMap, halt, txt, vBox)
import Brick.BChan (BChan)
import Brick.Widgets.Border (border)
import Control.Lens ((%=))
import Data.IORef (IORef, newIORef)
import Graphics.Vty (ColorMode (..), Event (..), Key (..), Modifier (..), defAttr)
import Graphics.Vty.Platform.Unix.Input.Loop (initInput)
import Graphics.Vty.Platform.Unix.Input.Terminfo (classifyMapForTerm)
import qualified Graphics.Vty.Platform.Unix.Output.XTermColor as XTermColor
import Graphics.Vty.Platform.Unix.Settings (UnixSettings (..))
import qualified System.Console.Terminfo.Base as Terminfo
import System.IO (Handle)
import System.Posix (Fd)

import qualified Hix.Console as Console
import Hix.Data.Monad (M)
import Hix.Handlers.Tui (RunBrick (..), TuiHandlers (..))
import Hix.Handlers.Tui.Prod (handlersProd, runBrickWith)
import Hix.Terminal.State (VtyTuiResources (..))
import Hix.Ui.Debug (BrickDebugEvent (..))

-- | Run a Brick app with a @Vty@ customized for operation through named pipes, used in tests with a @tmux@ server
-- running on the other side.
--
-- This entails calling 'initInput' instead of the default @buildInput@, since the latter performs terminal
-- initialization steps that break the pipes.
--
-- In contrast to the native variant, this also disables querying the terminal size through system calls, since that
-- doesn't work with pipes.
-- Instead, the initial size obtained by sending terminfo codes is used statically.
--
-- 'Terminfo.setupTerm' is called explicitly here and once again in 'XTermColor.reserveTerminal', because we have to set
-- up the input sequence classification table required by 'initInput'.
runBrickTest ::
  Ord n =>
  String ->
  IORef VtyTuiResources ->
  App s e n ->
  Maybe (BChan e) ->
  s ->
  M s
runBrickTest term =
  runBrickWith False (Just term) \ settings -> do
    terminal <- Terminfo.setupTerm term
    input <- initInput settings (classifyMapForTerm term terminal)
    output <- XTermColor.reserveTerminal term settings.settingOutputFd FullColor
    pure (input, output)

handlersTestWith ::
  String ->
  Fd ->
  Fd ->
  Handle ->
  Handle ->
  IO (TuiHandlers M)
handlersTestWith term fdIn fdOut hIn hOut = do
  state <- newIORef VtyTuiResources {fdIn, fdOut, hIn, hOut}
  pure TuiHandlers {
    log = \ _ -> Console.err,
    runBrick = RunBrick (runBrickTest term state)
  }

handlersTestTmux ::
  Fd ->
  Fd ->
  Handle ->
  Handle ->
  IO (TuiHandlers M)
handlersTestTmux = handlersTestWith "tmux-256color"

vtyEvent :: Event -> EventM n Bool ()
vtyEvent = \case
  EvKey KEnter [] ->
    halt
  EvKey (KChar 't') [] ->
    id %= not
  EvKey (KChar 'c') [MCtrl] -> do
    halt
  _e -> do
    unit

handleEvent ::
  BrickEvent n BrickDebugEvent ->
  EventM n Bool ()
handleEvent = \case
  VtyEvent ve -> vtyEvent ve
  AppEvent (BrickVtyEvent ve) -> vtyEvent ve
  _e -> unit

renderTest :: Bool -> Widget ()
renderTest flag =
  if flag
  then
    border $
    vBox [
      txt "🛠️",
      txt "6",
      txt "7",
      txt "8"
    ]
  else
    vBox [
      txt "1",
      txt "2",
      txt "3",
      txt "4",
      txt "5"
    ]

tuiTestCli :: M ()
tuiTestCli = do
  handlers <- liftIO $ handlersProd
  let RunBrick run = handlers.runBrick
  void $ run app Nothing False
  where
    app =
      App {
        appDraw = \ s -> [renderTest s],
        appStartEvent = unit,
        appHandleEvent = handleEvent,
        appChooseCursor = \ _ _ -> Nothing,
        appAttrMap = const (attrMap defAttr [])
      }
