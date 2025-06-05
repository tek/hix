module Hix.Managed.Handlers.ReleaseUi.VersionProblems.Run where

import Hix.Data.Monad (M)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi.VersionProblems.Context (isAccepted, uiVersionProblems)
import Hix.Managed.Handlers.ReleaseUi.VersionProblems.Event (versionProblemsActions)
import Hix.Managed.Handlers.ReleaseUi.VersionProblems.KeyEvent (
  versionProblemsHandlerSpecs,
  versionProblemsHelpEvents,
  versionProblemsHelpHandlers,
  versionProblemsKeyConfig,
  )
import Hix.Managed.Handlers.ReleaseUi.VersionProblems.Render (render)
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow)
import Hix.Managed.Release.Validation (ProblematicVersion)
import Hix.Ui (ScreenConfig (..), runScreenWithHelp)
import Hix.Ui.Debug (BrickDebug)
import Hix.Ui.KeyMappings (HelpConfig (..))

chooseVersionProblems ::
  Maybe BrickDebug ->
  Packages ProblematicVersion ->
  M (Either TerminateFlow Bool)
chooseVersionProblems debug problems = do
  result <- runScreenWithHelp screenConfig
  pure (Right (isAccepted result))
  where
    screenConfig = ScreenConfig {
      name = "versionProblems",
      help = HelpConfig {
        keyConfig = versionProblemsKeyConfig,
        helpHandlers = versionProblemsHelpHandlers,
        helpEvents = versionProblemsHelpEvents
      },
      instructions = const "Version validation found problems",
      handlerSpecs = versionProblemsHandlerSpecs versionProblemsActions,
      render,
      startEvent = unit,
      debug,
      initialContext = uiVersionProblems problems
    }
