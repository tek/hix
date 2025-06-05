module Hix.Test.Managed.ReleaseUiTest where

import Control.Lens (Traversal')
import Control.Lens.At (ix)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get)
import Data.Sequence ((<|))
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Distribution.Version (mkVersion)
import Hedgehog (TestT, (===))

import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Handlers.ReleaseUi.Versions.State (
  Components (..),
  PackageVersion (..),
  TogglableVersion (..),
  VersionsContext,
  VersionsScreen (..),
  )
import Hix.Test.Utils (UnitTest)

import Hix.Ui.Data.Nav (
  ActiveRow (..),
  Focusable (..),
  FocusableRow (..),
  Grid (..),
  NavContext (..),
  NavMeta (..),
  Row (..),
  Tile (..),
  Tiles (..),
  focusable,
  navContext,
  unfocusRow,
  )
import Hix.Ui.Nav (navigateRow, navigateRows)

checkCur ::
  HasCallStack =>
  (Int, Int) ->
  (Int, Int) ->
  StateT VersionsContext (TestT IO) ()
checkCur (stateRow, stateCol) gridTarget =
  withFrozenCallStack do
    s <- get
    gridTarget === gridIndex s.navGrid
    Just True === stateFocused s.state
    6 === length (gridTiles s.navGrid)
  where
    stateFocused s = do
      c <- focusedVersion s
      Focusable {focused} <-
        if stateCol == 0
        then Just c.super
        else c.subs Vector.!? (stateCol - 1)
      pure focused

    focusedVersion VersionsScreen {shared, packages} =
      if stateRow == 0
      then Just shared.state.components
      else (.version.state.components) <$> packages Vector.!? (stateRow - 1)

    gridIndex Grid {pre, focus = FocusableRow {row}} = (length pre, rowCol row)

    rowCol (ActiveRow {pre = preRow}) = length preRow.tiles
    rowCol OnlyRow = 0

    gridTiles Grid {focus} = (unfocusRow focus).row.tiles

comp :: Int -> Vector Int -> Components
comp h t =
  Components {
    super = focusable h,
    subs = focusable <$> t
  }

pkg :: LocalPackage -> Int -> Vector Int -> PackageVersion
pkg package h t =
  PackageVersion {
    package,
    current = mkVersion (h : toList t),
    version = focusable TogglableVersion {
      enabled = True,
      components = comp h t
    }
  }

comps ::
  Bool ->
  Traversal' VersionsScreen (Focusable TogglableVersion) ->
  (NavMeta VersionsScreen Int, Seq (Tile VersionsScreen Int))
comps short l =
  (
    NavMeta {index = 0, lens = l . #state . #components . #super},
    [
      TileFocusable NavMeta {index = i + 1, lens = l . #state . #components . #subs . ix i}
      | i <- occupied
    ] <>
    (TileUnfocusable <$ empties)
  )
  where
    (occupied, empties) = if short then ([0..1], [(), ()]) else ([0..3], [])

compsRow ::
  Bool ->
  Traversal' VersionsScreen (Focusable TogglableVersion) ->
  Seq (Tile VersionsScreen Int)
compsRow short l = let
    (h, t) = comps short l
  in TileFocusable h <| TileUnfocusable <| t

test_nav :: UnitTest
test_nav =
  flip evalStateT s0 do
    checkCur (0, 0) (0, 1)
    right
    checkCur (0, 1) (0, 2)
    down
    checkCur (1, 1) (1, 2)
    right
    checkCur (1, 2) (1, 3)
    right
    checkCur (1, 3) (1, 4)
    right
    checkCur (1, 4) (1, 5)
    right
    checkCur (1, 0) (1, 0)
    left
    checkCur (1, 4) (1, 5)
    left
    checkCur (1, 3) (1, 4)
    right
    checkCur (1, 4) (1, 5)
    right
    checkCur (1, 0) (1, 0)
    up
    checkCur (0, 0) (0, 1)
    up
    checkCur (4, 0) (4, 0)
    left
    checkCur (4, 2) (4, 3)
    left
    checkCur (4, 1) (4, 2)
    right
    checkCur (4, 2) (4, 3)
    right
    checkCur (4, 0) (4, 0)
    left
    checkCur (4, 2) (4, 3)
  where
    right = navigateRow True

    left = navigateRow False

    down = navigateRows True

    up = navigateRows False

    s0 = navContext state navGrid

    state = VersionsScreen {
      shared = focusable TogglableVersion {
        enabled = True,
        components = Components {
          super = focusable 1,
          subs = focusable <$> [1, 0, 0, 0]
        }
      },
      packages = [
        pkg "p1" 1 [0, 1, 0, 0],
        pkg "p2" 1 [0, 2, 0, 0],
        pkg "p3" 1 [0, 3],
        pkg "p4" 1 [0, 4]
      ]
    }

    navGrid = Grid {
      pre = [],
      focus = let
        (focus, post) = comps False #shared
      in FocusableRow {
        meta = NavMeta {index = 0, lens = #shared},
        row = ActiveRow {
          pre = Tiles [TileUnfocusable],
          focus,
          post = Tiles post
        }
      },
      post = let
        lens :: Int -> Traversal' VersionsScreen (Focusable TogglableVersion)
        lens i = #packages . ix i . #version
      in [
        RowFocusable (FocusableRow {meta = NavMeta {index = i + 1, lens = lens i}, row = Tiles (compsRow False (lens i))})
        | i <- [0..1]
      ] <> [
        RowFocusable (FocusableRow {meta = NavMeta {index = i + 1, lens = lens i}, row = Tiles (compsRow True (lens i))})
        | i <- [2..3]
      ]
    }
