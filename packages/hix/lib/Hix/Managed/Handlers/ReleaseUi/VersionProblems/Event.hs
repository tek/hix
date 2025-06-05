module Hix.Managed.Handlers.ReleaseUi.VersionProblems.Event where

import Brick (EventM, halt, modify)

import Hix.Managed.Handlers.ReleaseUi.VersionProblems.KeyEvent (VersionProblemsActions (..))
import Hix.Managed.Handlers.ReleaseUi.VersionProblems.State (VersionProblemsContext, VersionProblemsScreen (..))
import Hix.Ui.Data.Nav (Focusable (..), NavContext (..))

-- | Toggle acceptance.
setAccepted :: Bool -> EventM n VersionProblemsContext ()
setAccepted newVal =
  modify \ NavContext {state = screen, navGrid} ->
    let Focusable _ foc = screen.accepted
    in NavContext {state = screen {accepted = Focusable newVal foc}, navGrid}

versionProblemsActions :: VersionProblemsActions n VersionProblemsContext
versionProblemsActions =
  VersionProblemsActions {
    acceptProblems = setAccepted True >> halt,
    rejectProblems = setAccepted False >> halt
  }
