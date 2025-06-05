module Hix.Ui.Render where

import Brick (AttrName, Widget, attrName, txt, withAttr, withDefAttr)

import Hix.Ui.Data.Attr (brickAttr)

-- | Apply focus attribute when the row is focused.
rowFocusAttr :: Bool -> Widget a -> Widget a
rowFocusAttr focused =
  if focused
  then withDefAttr (attrName "focused")
  else id

-- | Render a toggle bullet indicator.
-- Shows a filled circle when focused, empty when not.
toggleBullet :: Bool -> Text
toggleBullet focused =
  if focused
  then "●"
  else "○"

-- | Attribute for toggle bullets based on selection state.
-- Selected gets green, enabled gets red.
toggleBulletAttr :: Bool -> AttrName
toggleBulletAttr =
  brickAttr . \case
    True -> "bullet" <> "selected"
    False -> "bullet" <> "enabled"

-- | Render a toggle bullet with appropriate styling.
renderToggleBullet :: Bool -> Bool -> Widget n
renderToggleBullet selected focused =
  withAttr (toggleBulletAttr selected) (txt (toggleBullet focused))
