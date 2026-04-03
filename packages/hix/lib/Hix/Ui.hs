module Hix.Ui (
  UiName,
  ScreenConfig (..),
  runScreen,
  runScreenWithHelp,
  runUiApp,
  emptyCell,
) where

import Brick (
  App (..),
  BrickEvent (..),
  CursorLocation,
  EventM,
  Padding (..),
  Widget,
  attrName,
  hBox,
  halt,
  nestEventM',
  padRight,
  txt,
  vBox,
  withAttr,
  )
import Brick.Keybindings (KeyConfig)
import Brick.Widgets.Border (border)
import Control.Lens (use, (%=), (.=))
import Exon (exon)

import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Handlers.Tui (RunBrick (..), TuiHandlers (..))
import Hix.Monad (clientError)
import Hix.Ui.Data.Attr (defaultAttrMap)
import Hix.Ui.Data.KeyBinding (
  AppState (..),
  KeyHandlerSpec (..),
  UiKeyActions (..),
  UiKeyEvent (..),
  commonHandlerSpecs,
  makeKeyDispatcher,
  )
import Hix.Ui.Data.Nav (NavContext (..))
import Hix.Ui.Debug (BrickDebug (..), BrickDebugEvent (..), brickAppDebug)
import Hix.Ui.Event (handleEvent)
import Hix.Ui.KeyMappings (HelpConfig (..), withHelpScreen)
import Hix.Ui.Nav (navigateRow, navigateRows)

runBrickApp ::
  Ord n =>
  s ->
  App s BrickDebugEvent n ->
  Maybe BrickDebug ->
  M s
runBrickApp s app debug = do
  TuiHandlers {runBrick = RunBrick run} <- appRes.tui
  run (maybe id brickAppDebug debug app) ((.input) <$> debug) s

-- | Resource name for extent tracking
data UiName
  deriving stock (Eq, Ord, Show)

noCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
noCursor _ _ = Nothing

emptyCell :: Widget n
emptyCell =
  txt " "

-- | Run a UI application with standard configuration.
runUiApp ::
  Ord n =>
  (s -> Widget n) ->
  EventM n s () ->
  (BrickEvent n BrickDebugEvent -> EventM n s ()) ->
  Maybe BrickDebug ->
  s ->
  M s
runUiApp render appStartEvent appHandleEvent debug initialContext =
  runBrickApp initialContext app debug
  where
    app =
      App {
        appDraw = \ s -> [border (render s)],
        appStartEvent,
        appHandleEvent,
        appChooseCursor = noCursor,
        appAttrMap = const defaultAttrMap
      }

-- | Run a UI screen with key dispatcher infrastructure.
-- Returns the final state after the user completes the screen.
runScreen ::
  ∀ e n s .
  Ord e =>
  Ord n =>
  Text ->
  KeyConfig e ->
  [KeyHandlerSpec e n s] ->
  (s -> Widget n) ->
  EventM n s () ->
  Maybe BrickDebug ->
  s ->
  M s
runScreen name config specs render startEvent debug initialContext = do
  dispatcher <- either bindingConflict pure (makeKeyDispatcher config specs)
  runUiApp render startEvent (handleEvent dispatcher) debug initialContext
  where
    bindingConflict _ = clientError [exon|Key binding conflict in #{name} UI|]

-- | Render an instruction headline with a purple square indicator.
-- Used for step instructions in the release UI.
headline :: Text -> Widget n
headline text =
  hBox [
    withAttr (attrName "instruction" <> attrName "indicator") (padRight (Pad 1) (txt "██")),
    withAttr (attrName "instruction") (txt text)
  ]

-- | Configuration for a UI screen with help screen infrastructure.
data ScreenConfig e m a r t =
  ScreenConfig {
    name :: Text,
    help :: HelpConfig e m,
    -- | Instruction text for the headline (function of screen state for dynamic headlines)
    instructions :: a -> Text,
    -- | Screen-specific handler specs only (common handlers added by 'runScreenWithHelp')
    handlerSpecs :: [KeyHandlerSpec e UiName (NavContext a r t)],
    -- | Render function returning widget (height tracked via extents)
    render :: a -> Widget UiName,
    startEvent :: EventM UiName (NavContext a r t) (),
    debug :: Maybe BrickDebug,
    initialContext :: NavContext a r t
  }

-- Common navigation actions, generic for all screens.
commonActions :: UiKeyActions UiName () (NavContext a r t)
commonActions =
  UiKeyActions {
    navDown = navigateRows True,
    navUp = navigateRows False,
    navRight = navigateRow True,
    navLeft = navigateRow False,
    confirm = halt
  }

-- Lift screen handlers to 'AppState'.
-- We manually handle the state nesting since 'EventM' doesn't have a simple @Zoom@ instance.
liftAction :: EventM UiName (NavContext a r t) () -> EventM UiName (AppState (NavContext a r t)) ()
liftAction screenAction = do
  ctx <- use #context
  newContext <- nestEventM' ctx screenAction
  #context .= newContext

handlers ::
  [KeyHandlerSpec (UiKeyEvent e) UiName (NavContext s r t)] ->
  [KeyHandlerSpec (UiKeyEvent e) UiName (AppState (NavContext s r t))]
handlers screenHandlers =
  quitSpec :
  showHelpSpec :
  (liftHandlerSpec <$> (commonHandlerSpecs commonActions ++ screenHandlers))
  where
    quitSpec = KeyHandlerSpec Quit "Quit" (#quit .= True >> halt)

    showHelpSpec = KeyHandlerSpec ShowHelp "Toggle help" (#showingHelp %= not)

    liftHandlerSpec KeyHandlerSpec {..} =
      KeyHandlerSpec {event, description, action = liftAction action}

-- | Run a UI screen with help screen infrastructure.
-- Wraps the render function with a help screen toggle.
-- Uses Brick extents to track content height for key mappings positioning.
-- Lifts the screen's handlers to AppState and adds a 'ShowHelp' handler.
-- Returns @Nothing@ when the user quits (ctrl-c or q), @Just a@ when confirmed.
runScreenWithHelp ::
  ∀ e a r t m .
  Ord (UiKeyEvent e) =>
  Show (UiKeyEvent e) =>
  ScreenConfig (UiKeyEvent e) m a r t ->
  M (Maybe a)
runScreenWithHelp ScreenConfig {name, help, instructions, handlerSpecs, render, startEvent, debug, initialContext} = do
  dispatcher <- either bindingConflict pure (makeKeyDispatcher help.keyConfig appHandlerSpecs)
  result <- runUiApp renderWithHelp appStartEvent (handleEvent dispatcher) debug initialState
  pure if result.quit then Nothing else Just result.context.state
  where
    initialState = AppState {context = initialContext, showingHelp = False, quit = False}

    appHandlerSpecs = handlers handlerSpecs

    appStartEvent = liftAction startEvent

    renderWithHelp appState =
      withHelpScreen help appState.showingHelp $
      vBox [headline (instructions screenState), emptyCell, render screenState]
      where
        screenState = appState.context.state

    bindingConflict _ = clientError [exon|Key binding conflict in #{name} UI|]
