module Hix.Managed.Handlers.ReleaseUi.Prod where

import Hix.Managed.Handlers.ReleaseUi (ReleaseUi (..))
import Hix.Managed.Handlers.ReleaseUi.DistTargets.Run (chooseDistTargets)
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.Run (chooseUploadTargets)
import Hix.Managed.Handlers.ReleaseUi.VersionProblems.Run (chooseVersionProblems)
import Hix.Managed.Handlers.ReleaseUi.Versions.Run (chooseVersions)
import Hix.Ui.Debug (BrickDebug)

handlersProdWithEvents ::
  Maybe BrickDebug ->
  ReleaseUi
handlersProdWithEvents debug =
  ReleaseUi {
    chooseVersions = chooseVersions debug,
    chooseUploadTargets = chooseUploadTargets debug,
    chooseDistTargets = chooseDistTargets debug,
    chooseVersionProblems = chooseVersionProblems debug
  }

handlersProd :: ReleaseUi
handlersProd =
  handlersProdWithEvents Nothing
