module Hix.Managed.Handlers.ReleaseUi.Versions.Run where

import Hix.Data.Monad (M)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi.Versions.Context (chosenVersions, uiVersions)
import Hix.Managed.Handlers.ReleaseUi.Versions.Event (versionsActions)
import Hix.Managed.Handlers.ReleaseUi.Versions.KeyEvent (
  versionsHelpEvents,
  versionsHelpHandlers,
  versionsKeyConfig,
  versionsHandlerSpecs,
  )
import Hix.Managed.Handlers.ReleaseUi.Versions.Render (render)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget)
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion)
import Hix.Managed.Release.Data.VersionChoice (VersionChoice)
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow (..))
import Hix.Ui (ScreenConfig (..), runScreenWithHelp)
import Hix.Ui.Debug (BrickDebug)
import Hix.Ui.KeyMappings (HelpConfig (..))

chooseVersions ::
  Maybe BrickDebug ->
  Maybe SelectedVersion ->
  Packages ConfiguredTarget ->
  M (Either TerminateFlow VersionChoice)
chooseVersions debug sharedVersion targets = do
  runScreenWithHelp screenConfig <&> \case
    Nothing -> Left "User quit"
    Just result -> Right (chosenVersions result)
  where
    screenConfig = ScreenConfig {
      name = "versions",
      help = HelpConfig {
        keyConfig = versionsKeyConfig,
        helpHandlers = versionsHelpHandlers,
        helpEvents = versionsHelpEvents
      },
      instructions = const "Choose release versions",
      handlerSpecs = versionsHandlerSpecs versionsActions,
      render,
      startEvent = unit,
      debug,
      initialContext = uiVersions sharedVersion targets,
      screenLog = "Completed version selection"
    }
