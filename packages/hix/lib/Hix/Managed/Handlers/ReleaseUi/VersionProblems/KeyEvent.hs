module Hix.Managed.Handlers.ReleaseUi.VersionProblems.KeyEvent where

import Brick (EventM)
import Brick.Keybindings (Binding, KeyConfig, KeyDispatcher, KeyEventHandler, KeyEvents, bind, keyEvents, newKeyConfig)
import Graphics.Vty (Key (..))

import Hix.Ui.Data.KeyBinding (
  BindingConflicts,
  KeyHandlerSpec (..),
  UiKeyEvent (..),
  keyHandlersFromSpecs,
  makeKeyDispatcher,
  )

data VersionProblemsEvent =
  AcceptProblems
  |
  RejectProblems
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type VersionProblemsKeyEvent = UiKeyEvent VersionProblemsEvent

data VersionProblemsActions n s =
  VersionProblemsActions {
    acceptProblems :: EventM n s (),
    rejectProblems :: EventM n s ()
  }

versionProblemsKeyEventDefs :: [(Text, VersionProblemsKeyEvent)]
versionProblemsKeyEventDefs =
  [
    ("accept-problems", Specific AcceptProblems),
    ("reject-problems", Specific RejectProblems)
  ]

versionProblemsKeyEvents :: KeyEvents VersionProblemsKeyEvent
versionProblemsKeyEvents = keyEvents versionProblemsKeyEventDefs

versionProblemsDefaultBindings :: [(VersionProblemsKeyEvent, [Binding])]
versionProblemsDefaultBindings =
  [
    (Specific AcceptProblems, [bind (KChar 'y')]),
    (Specific RejectProblems, [bind (KChar 'n'), bind (KChar 'q')])
  ]

versionProblemsKeyConfig :: KeyConfig VersionProblemsKeyEvent
versionProblemsKeyConfig =
  newKeyConfig versionProblemsKeyEvents versionProblemsDefaultBindings []

versionProblemsHandlerSpecs ::
  VersionProblemsActions n s ->
  [KeyHandlerSpec VersionProblemsKeyEvent n s]
versionProblemsHandlerSpecs VersionProblemsActions {..} =
  [
    KeyHandlerSpec (Specific AcceptProblems) "Accept problems and continue" acceptProblems,
    KeyHandlerSpec (Specific RejectProblems) "Reject and abort release" rejectProblems
  ]

versionProblemsHelpSpecs :: [KeyHandlerSpec VersionProblemsKeyEvent n s]
versionProblemsHelpSpecs =
  [
    KeyHandlerSpec (Specific AcceptProblems) "Accept problems and continue" unit,
    KeyHandlerSpec (Specific RejectProblems) "Reject and abort release" unit
  ]

versionProblemsHelpHandlers :: [KeyEventHandler VersionProblemsKeyEvent (EventM n s)]
versionProblemsHelpHandlers =
  keyHandlersFromSpecs versionProblemsHelpSpecs

versionProblemsKeyDispatcher ::
  VersionProblemsActions n s ->
  Either (BindingConflicts VersionProblemsKeyEvent (EventM n s)) (KeyDispatcher VersionProblemsKeyEvent (EventM n s))
versionProblemsKeyDispatcher actions =
  makeKeyDispatcher versionProblemsKeyConfig (versionProblemsHandlerSpecs actions)

-- | Key events to display in the help corner
versionProblemsHelpEvents :: [(VersionProblemsKeyEvent, Text)]
versionProblemsHelpEvents =
  [
    (Specific AcceptProblems, "accept"),
    (Specific RejectProblems, "reject"),
    (Quit, "quit")
  ]
