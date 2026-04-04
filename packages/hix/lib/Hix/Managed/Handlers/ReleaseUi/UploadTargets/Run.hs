module Hix.Managed.Handlers.ReleaseUi.UploadTargets.Run where

import Exon (exon)

import Hix.Class.Map (nList)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Cabal.Data.UploadStage (UploadMutability (..), UploadStage (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.Context (chosenPackages, uiUploadTargets)
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.Event (uploadTargetsActions)
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.KeyEvent (
  uploadTargetsHandlerSpecs,
  uploadTargetsHelpEvents,
  uploadTargetsHelpHandlers,
  uploadTargetsKeyConfig,
  )
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.Render (render)
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.State (UploadTargetScreen (..))
import Hix.Managed.Release.Data.Staged (PreparedTargetView)
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow)
import Hix.Pretty (showP)
import Hix.Ui (ScreenConfig (..), runScreenWithHelp)
import Hix.Ui.Debug (BrickDebug)
import Hix.Ui.KeyMappings (HelpConfig (..))

chooseUploadTargetsNe ::
  Maybe BrickDebug ->
  UploadStage ->
  NonEmpty (LocalPackage, PreparedTargetView) ->
  M (Either TerminateFlow (Set LocalPackage))
chooseUploadTargetsNe debug stage packages = do
  runScreenWithHelp screenConfig <&> \case
    Nothing -> Left "User quit"
    Just result -> Right (chosenPackages result)
  where
    screenConfig = ScreenConfig {
      name = "uploadTargets",
      help = HelpConfig {
        keyConfig = uploadTargetsKeyConfig,
        helpHandlers = uploadTargetsHelpHandlers,
        helpEvents = uploadTargetsHelpEvents
      },
      instructions = \ UploadTargetScreen {stage = UploadStage {mutability, artifact}} ->
        "Select packages " <> case mutability of
          UploadCandidate -> "for which to upload candidates for " <> showP artifact
          UploadPublish -> "for which to publish " <> showP artifact,
      handlerSpecs = uploadTargetsHandlerSpecs uploadTargetsActions,
      render,
      startEvent = unit,
      debug,
      initialContext = uiUploadTargets stage packages,
      screenLog = [exon|Completed #{showP stage} target selection|]
    }

chooseUploadTargets ::
  Maybe BrickDebug ->
  UploadStage ->
  Packages PreparedTargetView ->
  M (Either TerminateFlow (Set LocalPackage))
chooseUploadTargets debug stage =
  maybe (pure (Right mempty)) (chooseUploadTargetsNe debug stage) . nonEmpty . nList
