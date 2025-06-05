module Hix.Managed.Handlers.ReleaseUi.DistTargets.Run where

import Hix.Class.Map (nList)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi.DistTargets.Context (chosenPackages, uiDistTargets)
import Hix.Managed.Handlers.ReleaseUi.DistTargets.Event (distTargetsActions)
import Hix.Managed.Handlers.ReleaseUi.DistTargets.KeyEvent (
  distTargetsHandlerSpecs,
  distTargetsHelpEvents,
  distTargetsHelpHandlers,
  distTargetsKeyConfig,
  )
import Hix.Managed.Handlers.ReleaseUi.DistTargets.Render (render)
import Hix.Managed.Handlers.ReleaseUi.DistTargets.State (DistTargetScreen (..))
import Hix.Managed.Release.Data.Staged (SelectedTargetView)
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow)
import Hix.Ui (ScreenConfig (..), runScreenWithHelp)
import Hix.Ui.Debug (BrickDebug)
import Hix.Ui.KeyMappings (HelpConfig (..))

chooseDistTargetsNe ::
  Maybe BrickDebug ->
  Bool ->
  NonEmpty (LocalPackage, SelectedTargetView) ->
  M (Either TerminateFlow (Set LocalPackage))
chooseDistTargetsNe debug checksPassed packages = do
  result <- runScreenWithHelp screenConfig
  pure (Right (chosenPackages result))
  where
    screenConfig = ScreenConfig {
      name = "distTargets",
      help = HelpConfig {
        keyConfig = distTargetsKeyConfig,
        helpHandlers = distTargetsHelpHandlers,
        helpEvents = distTargetsHelpEvents
      },
      instructions = \ DistTargetScreen {checksPassed = passed} ->
        if passed
        then "Select packages for which to create release distributions"
        else "Select packages for which to force release despite check failures",
      handlerSpecs = distTargetsHandlerSpecs distTargetsActions,
      render,
      startEvent = unit,
      debug,
      initialContext = uiDistTargets checksPassed packages
    }

chooseDistTargets ::
  Maybe BrickDebug ->
  Bool ->
  Packages SelectedTargetView ->
  M (Either TerminateFlow (Set LocalPackage))
chooseDistTargets debug checksPassed =
  maybe (pure (Right mempty)) (chooseDistTargetsNe debug checksPassed) . nonEmpty . nList
