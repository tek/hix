module Hix.Managed.Handlers.ReleaseUi.DistTargets.Render where

import Brick (Padding (..), Widget, attrName, hBox, padRight, txt, vBox, withAttr)

import Hix.Managed.Handlers.ReleaseUi.DistTargets.State (DistTarget (..), DistTargetScreen (..))
import Hix.Pretty (showP)
import Hix.Ui (UiName)
import Hix.Ui.Data.Nav (Focusable (..))
import Hix.Ui.Render (renderToggleBullet, rowFocusAttr)

renderPackage :: DistTarget -> Widget n
renderPackage DistTarget {..} =
  hBox (rowFocusAttr enabled.focused <$> parts)
  where
    parts =
      [
        padRight (Pad 1) (renderToggleBullet enabled.state enabled.focused),
        padRight (Pad 1) (txt (showP package)),
        withAttr (attrName "muted") (txt (showP version))
      ]

checksStatus :: Bool -> Text
checksStatus = \case
  True -> "Flake checks passed."
  False -> "Flake checks failed!"

render :: DistTargetScreen -> Widget UiName
render DistTargetScreen {checksPassed, packages} =
  vBox (statusLine : (renderPackage <$> toList packages))
  where
    statusLine =
      padRight (Pad 1) (withAttr statusAttr (txt (checksStatus checksPassed)))

    statusAttr =
      if checksPassed
      then attrName "success"
      else attrName "error"
