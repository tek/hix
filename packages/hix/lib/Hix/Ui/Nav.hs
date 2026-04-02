module Hix.Ui.Nav where

import Control.Lens (use, (.=))
import Control.Monad.State (MonadState)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), (<|), (|>))

import Hix.Ui.Data.Nav (
  ActiveRow (..),
  FocusableRow (..),
  FocusedRow,
  Grid (..),
  NavContext,
  NavHandlers (..),
  NavMeta,
  NewFocus (..),
  OldFocus (..),
  Row (..),
  Tile (..),
  Tiles (..),
  unfocusRow,
  updateFocusedRow,
  updateFocusedTile,
  )


-- | Find the first element in @candidates@ that can be focused and return it alongside the prefix and suffix enclosing
-- it.
-- @pre@ and @post@ are pre- and appended to the prefix and suffix in the result.
scanSegments ::
  ∀ grid meta focus elem result .
  NavHandlers grid meta focus elem result ->
  focus ->
  Seq elem ->
  Seq elem ->
  Seq elem ->
  Maybe (focus, Seq elem, Seq elem)
scanSegments handlers focus pre post candidates =
  spin [] candidates
  where
    spin scanned = \case
      Seq.Empty -> Nothing
      candidate :<| remaining ->
        tryCandidate scanned candidate remaining
        <|>
        spin (scanned |> candidate) remaining

    tryCandidate scanned candidate remaining =
      handlers.tryFocus (OldFocus focus) (NewFocus candidate) <&> \ newFocus ->
        (newFocus, pre <> scanned, remaining <> post)

navigate ::
  ∀ grid meta focus elem result .
  NavHandlers grid meta focus elem result ->
  Bool ->
  focus ->
  Seq elem ->
  Seq elem ->
  Maybe (focus, Seq elem, Seq elem)
navigate handlers forward focus =
  if forward then scan else mirrored
  where
    mirrored pre post =
      scan (Seq.reverse post) (Seq.reverse pre) <&> \ (newFocus, newPre, newPost) ->
        (newFocus, Seq.reverse newPost, Seq.reverse newPre)

    scan pre post =
      scanSegments handlers focus (pre |> handlers.unfocus focus) [] post
      <|>
      scanSegments handlers focus [] (handlers.unfocus focus <| post) pre

navigateState ::
  ∀ s r t meta focus elem result m .
  MonadState (NavContext s r t) m =>
  NavHandlers (Grid s r t) meta focus elem result ->
  Bool ->
  m ()
navigateState handlers next = do
  grid <- use #navGrid
  for_ (applyNav grid) \ newGrid -> do
    updateFocusedRow False
    updateFocusedTile False
    #navGrid .= newGrid
    updateFocusedTile True
    updateFocusedRow True
  where
    applyNav grid = do
      (meta, focus, pre, post) <- handlers.extract grid
      (newFocus, newPre, newPost) <- navigate handlers next focus pre post
      pure (handlers.inject grid meta newFocus newPre newPost)

navigateRow ::
  MonadState (NavContext s r t) m =>
  Bool ->
  m ()
navigateRow =
  navigateState handlers
  where
    handlers :: NavHandlers (Grid s r t) (NavMeta s r) (NavMeta s t) (Tile s t) (FocusedRow s r t)
    handlers =
      NavHandlers {
        extract,
        inject,
        tryFocus,
        unfocus = TileFocusable
      }

    extract Grid {focus = FocusableRow {meta, row}} = case row of
      ActiveRow {pre, focus, post} -> Just (meta, focus, pre.tiles, post.tiles)
      OnlyRow -> Nothing

    inject grid meta focus rowPre rowPost =
      Grid {
        pre = grid.pre,
        focus = FocusableRow {
          meta,
          row = ActiveRow {
            pre = Tiles rowPre,
            focus,
            post = Tiles rowPost
          }
        },
        post = grid.post
      }

    tryFocus _oldFocus (NewFocus candidate) =
      case candidate of
        TileFocusable focus -> Just focus
        TileUnfocusable -> Nothing


focusRow :: Int -> Tiles s t -> ActiveRow s t
focusRow targetIndex (Tiles tiles) =
  fromMaybe OnlyRow $
  uncurry (tryRight []) (Seq.splitAt targetIndex tiles)
  where
    tryRight between pre = \case
      TileFocusable focus :<| post ->
        pure ActiveRow {
          pre = Tiles (pre <> between),
          post = Tiles post,
          ..
        }
      tile :<| post -> tryLeft (between |> tile) post pre
      Seq.Empty -> tryLeft between [] pre

    tryLeft between post = \case
      pre :|> TileFocusable focus ->
        pure ActiveRow {
          pre = Tiles pre,
          post = Tiles (between <> post),
          ..
        }
      pre :|> tile -> tryRight (tile <| between) pre post
      Seq.Empty | Seq.Empty <- post -> Nothing
                | otherwise -> tryRight between [] post

navigateRows ::
  ∀ s r t m .
  MonadState (NavContext s r t) m =>
  Bool ->
  m ()
navigateRows =
  navigateState handlers
  where
    handlers :: NavHandlers (Grid s r t) () (FocusedRow s r t) (Row s r t) (Grid s r t)
    handlers =
      NavHandlers {
        extract,
        inject,
        tryFocus,
        unfocus = RowFocusable . unfocusRow
      }

    extract Grid {pre, focus, post} = Just ((), focus, pre, post)

    inject _oldGrid _meta focus pre post = Grid {..}

    tryFocus (OldFocus FocusableRow {row = oldFocus}) = \case
        NewFocus (RowFocusable row) ->
          Just (focusRow tileIndex <$> row)
          where
            tileIndex = case oldFocus of
              ActiveRow {pre = Tiles {tiles}} -> length tiles
              OnlyRow -> 0
        NewFocus RowUnfocusable -> Nothing
