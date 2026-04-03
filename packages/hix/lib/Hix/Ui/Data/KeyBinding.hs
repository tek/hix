-- | Generic key binding infrastructure for Brick UIs
module Hix.Ui.Data.KeyBinding where

import Brick (EventM)
import Brick.Keybindings (Binding (..), KeyConfig, KeyDispatcher, KeyEventHandler, KeyHandler, bind, keyDispatcher, onEvent)
import qualified Data.Set as Set
import Graphics.Vty (Key (..), Modifier (..))

-- | Conflicting bindings error type.
type BindingConflicts e m = [(Binding, [KeyHandler e m])]

data KeyHandlerSpec e n s =
  KeyHandlerSpec {
    event :: e,
    description :: Text,
    action :: EventM n s ()
  }

keyHandlersFromSpecs ::
  [KeyHandlerSpec e n s] ->
  [KeyEventHandler e (EventM n s)]
keyHandlersFromSpecs =
  fmap toHandler
  where
    toHandler KeyHandlerSpec {..} = onEvent event description action

-- | Key event handlers with dummy actions (for help display only).
helpHandlersFromSpecs ::
  [KeyHandlerSpec e n s] ->
  [KeyEventHandler e (EventM n' s')]
helpHandlersFromSpecs =
  fmap toHandler
  where
    toHandler KeyHandlerSpec {..} = onEvent event description unit

makeKeyDispatcher ::
  Ord e =>
  KeyConfig e ->
  [KeyHandlerSpec e n s] ->
  Either (BindingConflicts e (EventM n s)) (KeyDispatcher e (EventM n s))
makeKeyDispatcher config specs =
  keyDispatcher config (keyHandlersFromSpecs specs)

-- | App-level state wrapper that adds help UI state to the screen's 'NavContext'.
data AppState ctx =
  AppState {
    context :: ctx,
    showingHelp :: Bool,
    quit :: Bool
  }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Common key events shared across all UIs, parameterized by screen-specific events.
data UiKeyEvent a =
  NavigateDown
  | NavigateUp
  | NavigateRight
  | NavigateLeft
  | Confirm
  | Quit
  | ShowHelp
  | Specific a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Common key actions shared across all UIs.
-- Note: @ShowHelp@ is handled at the app level, not here.
data UiKeyActions n a s =
  UiKeyActions {
    navDown :: EventM n s (),
    navUp :: EventM n s (),
    navRight :: EventM n s (),
    navLeft :: EventM n s (),
    confirm :: EventM n s ()
  }

-------------------------------------------------------------------------------
-- Common event/action infrastructure
-------------------------------------------------------------------------------

data CommonEventMapping e n a s =
  CommonEventMapping {
    name :: Text,
    event :: e,
    description :: Text,
    binding :: Binding,
    action :: UiKeyActions n a s -> EventM n s ()
  }

-- | Standard common event mappings for navigation (fixed for all UIs).
-- Note: 'ShowHelp' has no action since it's handled at the app level.
commonEventMappings :: ∀ e n a s . [CommonEventMapping (UiKeyEvent e) n a s]
commonEventMappings =
  [
    CommonEventMapping "nav-down" NavigateDown "Move selection down" (bind (KChar 'j')) (.navDown),
    CommonEventMapping "nav-up" NavigateUp "Move selection up" (bind (KChar 'k')) (.navUp),
    CommonEventMapping "nav-right" NavigateRight "Move selection right" (bind (KChar 'l')) (.navRight),
    CommonEventMapping "nav-left" NavigateLeft "Move selection left" (bind (KChar 'h')) (.navLeft),
    CommonEventMapping "confirm" Confirm "Confirm selection" (bind KEnter) (.confirm)
  ]

-- | Extract key event definitions from common mappings (including 'ShowHelp').
commonKeyEventDefs :: ∀ a . [(Text, UiKeyEvent a)]
commonKeyEventDefs =
  fmap (\ CommonEventMapping {..} -> (name, event)) commonEventMappings ++
  [("quit", Quit),
   ("show-help", ShowHelp)]

-- | Extract default bindings from common mappings (including 'ShowHelp').
commonBindings :: ∀ a . [(UiKeyEvent a, [Binding])]
commonBindings =
  fmap (\ CommonEventMapping {..} -> (event, [binding])) commonEventMappings ++
  [(Quit, [bind (KChar 'q'), Binding {kbKey = KChar 'c', kbMods = Set.singleton MCtrl}]),
   (ShowHelp, [bind (KChar '?')])]

-- | Create handler specs from common mappings and actions.
commonHandlerSpecs :: ∀ e n a s .
  UiKeyActions n a s ->
  [KeyHandlerSpec (UiKeyEvent e) n s]
commonHandlerSpecs actions =
  fmap toSpec commonEventMappings
  where
    toSpec CommonEventMapping {..} = KeyHandlerSpec event description (action actions)

-- | Create help specs (dummy actions) from common mappings (including ShowHelp).
commonHelpSpecs :: ∀ a n s . [KeyHandlerSpec (UiKeyEvent a) n s]
commonHelpSpecs =
  fmap (\  CommonEventMapping {..} -> KeyHandlerSpec event description unit) commonEventMappings ++
  [KeyHandlerSpec Quit "Quit" unit,
   KeyHandlerSpec ShowHelp "Toggle help" unit]
