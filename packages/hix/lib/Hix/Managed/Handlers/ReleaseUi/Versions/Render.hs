module Hix.Managed.Handlers.ReleaseUi.Versions.Render where

import Brick (AttrName, Padding (..), Widget, attrName, hBox, padRight, txt, withAttr, withDefAttr)
import Brick.Widgets.Table (columnBorders, renderTable, rowBorders, surroundingBorder, table)

import Hix.Managed.Handlers.ReleaseUi.Versions.State (
  Components (..),
  PackageVersion (..),
  TogglableVersion (..),
  VersionsScreen (..),
  componentsFocused,
  )
import Hix.Pretty (showP)
import Hix.Ui (UiName, emptyCell)
import Hix.Ui.Data.Attr (brickAttr)
import Hix.Ui.Data.Nav (Focusable (..))
import Hix.Ui.Render (rowFocusAttr)

data RowState =
  RowDisabled
  |
  RowEnabled
  |
  RowSelected
  deriving stock (Eq, Show)

rowState :: Bool -> Bool -> RowState
rowState = \cases
  True True -> RowSelected
  True False -> RowEnabled
  False _ -> RowDisabled

focusTileAttr :: Bool -> Widget a -> Widget a
focusTileAttr focused =
  if focused
  then withDefAttr (brickAttr ("focused" <> "tile"))
  else id

renderComponent :: RowState -> Focusable Int -> Widget n
renderComponent row c =
  focusTileAttr c.focused $
  digitAttr row $
  txt (show @_ @Int c.state)

digitAttr :: RowState -> Widget a -> Widget a
digitAttr = \case
  RowDisabled -> id
  _ -> withAttr (attrName "version-digit")

renderVersion :: RowState -> Components -> Widget n
renderVersion row components =
  hBox (intersperse (txt ".") (renderComponent row <$> (components.super : toList components.subs)))

bullet :: Bool -> Text
bullet focused =
  if focused
  then "●"
  else "○"


bulletAttr :: RowState -> AttrName
bulletAttr =
  brickAttr . \case
    RowSelected -> "bullet" <> "selected"
    RowEnabled -> "bullet" <> "enabled"
    RowDisabled -> "bullet" <> "disabled"

rowDisabledAttr :: RowState -> Widget a -> Widget a
rowDisabledAttr = \case
  RowDisabled -> withDefAttr (brickAttr ("row" <> "disabled"))
  _ -> id


versionRow ::
  Text ->
  RowState ->
  Components ->
  [Widget n]
versionRow desc row components =
  rowFocusAttr focused . padRight (Pad 1) <$>
  [
    withAttr (bulletAttr row) (txt (bullet focused)),
    rowDisabledAttr row (txt desc),
    rowDisabledAttr row (renderVersion row components)
  ]
  where
    focused = componentsFocused components

render :: VersionsScreen -> Widget UiName
render VersionsScreen {shared = sharedFocusable, packages} =
  versionsTable
  where

    versionsTable =
      renderTable $
      surroundingBorder False $
      rowBorders False $
      columnBorders False $
      table rows

    rows
      | showShared = sharedRow : separatorRow : packageRows
      | otherwise = packageRows

    sharedRow =
      versionRow "All packages" (rowState shared.enabled shared.enabled) shared.components

    separatorRow =
      [emptyCell, emptyCell, emptyCell]

    packageRows =
      renderPackageVersion <$> toList packages

    renderPackageVersion PackageVersion {package, version = Focusable {state = TogglableVersion {enabled, components}}} =
      versionRow (showP package) (rowState (showShared && not shared.enabled) enabled) components

    showShared = length (toList packages) > 1

    shared = sharedFocusable.state
