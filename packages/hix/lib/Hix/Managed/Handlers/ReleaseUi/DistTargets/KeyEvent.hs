module Hix.Managed.Handlers.ReleaseUi.DistTargets.KeyEvent where

import Brick (EventM)
import Brick.Keybindings (Binding, KeyConfig, KeyEventHandler, KeyEvents, bind, keyEvents, newKeyConfig)
import Graphics.Vty (Key (..))

import Hix.Ui.Data.KeyBinding (
  KeyHandlerSpec (..),
  UiKeyEvent (..),
  commonBindings,
  commonHelpSpecs,
  commonKeyEventDefs,
  helpHandlersFromSpecs,
  )

data DistTargetsEvent =
  TogglePackage
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type DistTargetsKeyEvent = UiKeyEvent DistTargetsEvent

newtype DistTargetsActions n s =
  DistTargetsActions {
    togglePackage :: EventM n s ()
  }

distTargetsEventDefs :: [(Text, DistTargetsKeyEvent)]
distTargetsEventDefs =
  [
    ("toggle-package", Specific TogglePackage)
  ]

distTargetsKeyEvents :: KeyEvents DistTargetsKeyEvent
distTargetsKeyEvents =
  keyEvents (commonKeyEventDefs ++ distTargetsEventDefs)

distTargetsBindings :: [(DistTargetsKeyEvent, [Binding])]
distTargetsBindings =
  [
    (Specific TogglePackage, [bind (KChar 't')])
  ]

distTargetsDefaultBindings :: [(DistTargetsKeyEvent, [Binding])]
distTargetsDefaultBindings =
  commonBindings ++
  distTargetsBindings

distTargetsKeyConfig :: KeyConfig DistTargetsKeyEvent
distTargetsKeyConfig =
  newKeyConfig distTargetsKeyEvents distTargetsDefaultBindings []

distTargetsHandlerSpecs ::
  DistTargetsActions n s ->
  [KeyHandlerSpec DistTargetsKeyEvent n s]
distTargetsHandlerSpecs DistTargetsActions {..} =
  [
    KeyHandlerSpec (Specific TogglePackage) "Toggle package for release" togglePackage
  ]

distTargetsHelpSpecs :: [KeyHandlerSpec DistTargetsKeyEvent n s]
distTargetsHelpSpecs =
  [
    KeyHandlerSpec (Specific TogglePackage) "Toggle package for release" unit
  ]

distTargetsHelpHandlers :: [KeyEventHandler DistTargetsKeyEvent (EventM n s)]
distTargetsHelpHandlers =
  helpHandlersFromSpecs (commonHelpSpecs ++ distTargetsHelpSpecs)

-- | Key events to display in the help corner
distTargetsHelpEvents :: [(DistTargetsKeyEvent, Text)]
distTargetsHelpEvents =
  [
    (Specific TogglePackage, "toggle"),
    (ShowHelp, "help")
  ]
