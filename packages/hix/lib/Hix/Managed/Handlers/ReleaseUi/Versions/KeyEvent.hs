module Hix.Managed.Handlers.ReleaseUi.Versions.KeyEvent where

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

data VersionsEvent =
  ToggleShared
  |
  TogglePackage
  |
  BumpVersion
  |
  IncrementComponent
  |
  DecrementComponent
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type VersionsKeyEvent = UiKeyEvent VersionsEvent

data VersionsActions n s =
  VersionsActions {
    toggleShared :: EventM n s (),
    togglePackage :: EventM n s (),
    bumpVersion :: EventM n s (),
    incrementComponent :: EventM n s (),
    decrementComponent :: EventM n s ()
  }

versionsEvents :: [(Text, VersionsKeyEvent)]
versionsEvents =
  [
    ("toggle-shared", Specific ToggleShared),
    ("toggle-package", Specific TogglePackage),
    ("bump-version", Specific BumpVersion),
    ("increment-component", Specific IncrementComponent),
    ("decrement-component", Specific DecrementComponent)
  ]

versionsKeyEvents :: KeyEvents VersionsKeyEvent
versionsKeyEvents = keyEvents $
  commonKeyEventDefs ++
  versionsEvents

versionsBindings :: [(VersionsKeyEvent, [Binding])]
versionsBindings =
  [
    (Specific ToggleShared, [bind (KChar 's')]),
    (Specific TogglePackage, [bind (KChar 't')]),
    (Specific BumpVersion, [bind (KChar 'b')]),
    (Specific IncrementComponent, [bind (KChar 'i')]),
    (Specific DecrementComponent, [bind (KChar 'd')])
  ]

versionsDefaultBindings :: [(VersionsKeyEvent, [Binding])]
versionsDefaultBindings =
  commonBindings ++
  versionsBindings

versionsKeyConfig :: KeyConfig VersionsKeyEvent
versionsKeyConfig =
  newKeyConfig versionsKeyEvents versionsDefaultBindings []

versionsHandlerSpecs ::
  VersionsActions n s ->
  [KeyHandlerSpec VersionsKeyEvent n s]
versionsHandlerSpecs VersionsActions {..} =
  [
    KeyHandlerSpec (Specific ToggleShared) "Toggle shared version mode" toggleShared,
    KeyHandlerSpec (Specific TogglePackage) "Toggle package for release" togglePackage,
    KeyHandlerSpec (Specific BumpVersion) "Bump version at cursor" bumpVersion,
    KeyHandlerSpec (Specific IncrementComponent) "Increment version component" incrementComponent,
    KeyHandlerSpec (Specific DecrementComponent) "Decrement version component" decrementComponent
  ]

versionsHelpSpecs :: [KeyHandlerSpec VersionsKeyEvent n s]
versionsHelpSpecs =
  [
    KeyHandlerSpec (Specific ToggleShared) "Toggle shared version mode" unit,
    KeyHandlerSpec (Specific TogglePackage) "Toggle package for release" unit,
    KeyHandlerSpec (Specific BumpVersion) "Bump version at cursor" unit,
    KeyHandlerSpec (Specific IncrementComponent) "Increment version component" unit,
    KeyHandlerSpec (Specific DecrementComponent) "Decrement version component" unit
  ]

versionsHelpHandlers :: [KeyEventHandler VersionsKeyEvent (EventM n s)]
versionsHelpHandlers =
  helpHandlersFromSpecs $
  commonHelpSpecs ++
  versionsHelpSpecs


-- | Key events to display in the help corner (most important ones).
versionsHelpEvents :: [(VersionsKeyEvent, Text)]
versionsHelpEvents =
  [
    (Specific BumpVersion, "bump"),
    (Specific ToggleShared, "shared"),
    (ShowHelp, "help")
  ]
