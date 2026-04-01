module Hix.Managed.Handlers.ReleaseUi.UploadTargets.KeyEvent where

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

data UploadTargetsEvent =
  TogglePackage
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type UploadTargetsKeyEvent = UiKeyEvent UploadTargetsEvent

newtype UploadTargetsActions n s =
  UploadTargetsActions {
    togglePackage :: EventM n s ()
  }

uploadTargetsEventDefs :: [(Text, UploadTargetsKeyEvent)]
uploadTargetsEventDefs =
  [
    ("toggle-package", Specific TogglePackage)
  ]

uploadTargetsKeyEvents :: KeyEvents UploadTargetsKeyEvent
uploadTargetsKeyEvents =
  keyEvents (commonKeyEventDefs ++ uploadTargetsEventDefs)

uploadTargetsBindings :: [(UploadTargetsKeyEvent, [Binding])]
uploadTargetsBindings =
  [
    (Specific TogglePackage, [bind (KChar 't')])
  ]

uploadTargetsDefaultBindings :: [(UploadTargetsKeyEvent, [Binding])]
uploadTargetsDefaultBindings =
  commonBindings ++
  uploadTargetsBindings

uploadTargetsKeyConfig :: KeyConfig UploadTargetsKeyEvent
uploadTargetsKeyConfig =
  newKeyConfig uploadTargetsKeyEvents uploadTargetsDefaultBindings []

uploadTargetsHandlerSpecs ::
  UploadTargetsActions n s ->
  [KeyHandlerSpec UploadTargetsKeyEvent n s]
uploadTargetsHandlerSpecs UploadTargetsActions {..} =
  [
    KeyHandlerSpec (Specific TogglePackage) "Toggle package for upload" togglePackage
  ]

uploadTargetsHelpSpecs :: [KeyHandlerSpec UploadTargetsKeyEvent n s]
uploadTargetsHelpSpecs =
  [
    KeyHandlerSpec (Specific TogglePackage) "Toggle package for upload" unit
  ]

uploadTargetsHelpHandlers :: [KeyEventHandler UploadTargetsKeyEvent (EventM n s)]
uploadTargetsHelpHandlers =
  helpHandlersFromSpecs (commonHelpSpecs ++ uploadTargetsHelpSpecs)

-- | Key events to display in the help corner
uploadTargetsHelpEvents :: [(UploadTargetsKeyEvent, Text)]
uploadTargetsHelpEvents =
  [
    (Specific TogglePackage, "toggle"),
    (Quit, "quit"),
    (ShowHelp, "help")
  ]
