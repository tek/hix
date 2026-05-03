module Hix.Managed.Handlers.ReleaseUi.Versions.Event where

import Brick (EventM, get)
import Control.Lens ((%=), (%~))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Hix.Managed.Handlers.ReleaseUi.Versions.KeyEvent (VersionsActions (..))
import Hix.Managed.Handlers.ReleaseUi.Versions.State (
  PackageVersion,
  TogglableVersion (..),
  VersionsContext,
  VersionsScreen (..),
  packageVersionEnabled,
  withNonEmptyComponents,
  )
import Hix.Ui.Data.Nav (Focusable (..), FocusableRow (..), NavContext (..), NavMeta (..), focusedRow, focusedTile)
import Hix.Ui.Nav (navigateToRowIndex)

overFocusedRow ::
  (TogglableVersion -> TogglableVersion) ->
  EventM n VersionsContext ()
overFocusedRow f = do
  FocusableRow {meta = NavMeta {lens}} <- focusedRow
  #state . lens . #state %= f

overFocusedComponent ::
  (Int -> Int) ->
  EventM n VersionsContext ()
overFocusedComponent f =
  focusedTile >>= traverse_ \ NavMeta {lens} -> #state . lens . #state %= f

firstEnabledRow :: Vector PackageVersion -> Int
firstEnabledRow packages =
  fromMaybe 0 (Vector.findIndex packageVersionEnabled packages) + 1

toggleShared :: EventM n VersionsContext ()
toggleShared = do
  #state . #shared . #state . #enabled %= not
  NavContext {state = VersionsScreen {shared, packages}} <- get
  isShared <- isFocusedSharedRow
  case (shared.state.enabled, isShared) of
    (True, False) -> navigateToRowIndex 0 0
    (False, True) -> navigateToRowIndex (firstEnabledRow packages) 0
    _ -> unit

-- | Check if the focused row is the shared row (index 0)
isFocusedSharedRow :: EventM n VersionsContext Bool
isFocusedSharedRow = do
  FocusableRow {meta = NavMeta {index}} <- focusedRow
  pure (index == 0)

-- | Check if the focused row is disabled.
-- Shared row is disabled when shared mode is off.
-- Package rows are disabled when shared mode is on.
isFocusedRowDisabled :: EventM n VersionsContext Bool
isFocusedRowDisabled = do
  isSharedRow <- isFocusedSharedRow
  NavContext {state = VersionsScreen {shared}} <- get
  let sharedEnabled = shared.state.enabled
  pure
    if isSharedRow
    then not sharedEnabled
    else sharedEnabled

-- | Enable the focused row if it's currently disabled.
-- For shared row: enables shared mode.
-- For package row: disables shared mode (switches to individual).
enableRowIfDisabled :: EventM n VersionsContext ()
enableRowIfDisabled =
  whenM isFocusedRowDisabled toggleShared

togglePackage :: EventM n VersionsContext ()
togglePackage = do
  enableRowIfDisabled
  overFocusedRow (#enabled %~ not)

bumpPackage :: Int -> TogglableVersion -> TogglableVersion
bumpPackage target =
  withNonEmptyComponents (NonEmpty.zipWith bumpFocusable [0..])
  where
    bumpFocusable index = #state %~ bumpComponent index

    bumpComponent index version = if
      | index == target -> version + 1
      | index > target -> 0
      | otherwise -> version

bump :: EventM n VersionsContext ()
bump = do
  enableRowIfDisabled
  FocusableRow {meta = NavMeta {lens}} <- focusedRow
  focusedTile >>= traverse_ \ NavMeta {index} -> #state . lens . #state %= bumpPackage index

addToVersion :: Int -> EventM n VersionsContext ()
addToVersion diff = do
  enableRowIfDisabled
  overFocusedComponent \ version -> version + diff

versionsActions :: VersionsActions n VersionsContext
versionsActions =
  VersionsActions {
    toggleShared,
    togglePackage,
    bumpVersion = bump,
    incrementComponent = addToVersion 1,
    decrementComponent = addToVersion (-1)
  }
