module Hix.Managed.Handlers.ReleaseUi.UploadTargets.Event where

import Hix.Managed.Handlers.ReleaseUi.UploadTargets.KeyEvent (UploadTargetsActions (..))
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.State (UploadTargetContext)
import Hix.Ui.Data.Nav (overFocusedRow)

uploadTargetsActions :: UploadTargetsActions n UploadTargetContext
uploadTargetsActions =
  UploadTargetsActions {
    togglePackage = overFocusedRow not
  }
