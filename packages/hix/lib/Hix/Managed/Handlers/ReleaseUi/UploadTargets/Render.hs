module Hix.Managed.Handlers.ReleaseUi.UploadTargets.Render where

import Brick (Padding (..), Widget, attrName, hBox, padRight, txt, vBox, withAttr)

import Hix.Managed.Handlers.ReleaseUi.UploadTargets.State (UploadTarget (..), UploadTargetScreen (..))
import Hix.Pretty (showP)
import Hix.Ui (UiName)
import Hix.Ui.Data.Nav (Focusable (..))
import Hix.Ui.Render (renderToggleBullet, rowFocusAttr)

renderPackage :: UploadTarget -> Widget n
renderPackage UploadTarget {..} =
  hBox (rowFocusAttr enabled.focused <$> parts)
  where
    parts =
      [
        padRight (Pad 1) (renderToggleBullet enabled.state enabled.focused),
        padRight (Pad 1) (txt (showP package)),
        withAttr (attrName "muted") (txt (showP version))
      ]

render :: UploadTargetScreen -> Widget UiName
render UploadTargetScreen {packages} =
  vBox (renderPackage <$> toList packages)
