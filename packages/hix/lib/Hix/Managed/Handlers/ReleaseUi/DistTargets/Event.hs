module Hix.Managed.Handlers.ReleaseUi.DistTargets.Event where

import Hix.Managed.Handlers.ReleaseUi.DistTargets.KeyEvent (DistTargetsActions (..))
import Hix.Managed.Handlers.ReleaseUi.DistTargets.State (DistTargetContext)
import Hix.Ui.Data.Nav (overFocusedRow)

distTargetsActions :: DistTargetsActions n DistTargetContext
distTargetsActions =
  DistTargetsActions {togglePackage = overFocusedRow not}
