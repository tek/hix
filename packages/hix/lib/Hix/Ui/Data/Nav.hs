module Hix.Ui.Data.Nav where

import Control.Lens (Traversal', use, (%=), (%~), (.=))
import Control.Monad.State (MonadState)
import Data.Generics.Labels ()
import Data.Sequence ((|>))
import Exon (exon)
import GHC.Exts (IsList)

data Focusable a =
  Focusable {
    state :: a,
    focused :: Bool
  }
  deriving stock (Eq, Show, Generic)

focusable :: a -> Focusable a
focusable state =
  Focusable {state, focused = False}

setFocused :: Focusable a -> Focusable a
setFocused f = f {focused = True}

data NavMeta s a =
  NavMeta {
    index :: Int,
    lens :: Traversal' s (Focusable a)
  }

instance Show (NavMeta s a) where
  showsPrec d NavMeta {index} = showParen (d > 10) [exon|NavMeta { index :: #{showString (show index)} }|]

data Tile s t =
  TileUnfocusable
  |
  TileFocusable (NavMeta s t)
  deriving stock (Show)

tileFocusable :: Tile s t -> Bool
tileFocusable = \case
  TileFocusable _ -> True
  TileUnfocusable -> False

newtype Tiles s t =
  Tiles { tiles :: Seq (Tile s t) }
  deriving stock (Show)
  deriving newtype (IsList, Semigroup, Monoid)

data FocusableRow s r a =
  FocusableRow {
    meta :: NavMeta s r,
    row :: a
  }
  deriving stock (Show, Functor)

type UnfocusedRow s r t = FocusableRow s r (Tiles s t)

data ActiveRow s t =
  ActiveRow {
    pre :: Tiles s t,
    focus :: NavMeta s t,
    post :: Tiles s t
  }
  |
  OnlyRow
  deriving stock (Show)

type FocusedRow s r t = FocusableRow s r (ActiveRow s t)

data Row s r t =
  RowUnfocusable
  |
  RowFocusable (UnfocusedRow s r t)
  deriving stock (Show)

unfocusRow :: FocusedRow s r t -> UnfocusedRow s r t
unfocusRow FocusableRow {row, ..} =
  FocusableRow {row = tiles row, ..}
  where
    tiles = \case
      ActiveRow {..} -> Tiles ((pre.tiles |> TileFocusable focus) <> post.tiles)
      OnlyRow -> []

data Grid s r t =
  Grid {
    pre :: Seq (Row s r t),
    focus :: FocusedRow s r t,
    post :: Seq (Row s r t)
  }
  deriving stock (Show)

data NavContext s r t =
  NavContext {
    state :: s,
    navGrid :: Grid s r t
  }
  deriving stock (Show, Generic)

initStateFocus :: s -> Grid s r t -> s
initStateFocus state Grid {..} =
  setRow focus (setTile focus state)
  where
    setRow FocusableRow {meta = NavMeta {lens}} = lens %~ setFocused
    setTile = \case
      FocusableRow {row = ActiveRow {focus = NavMeta {lens}}} -> lens %~ setFocused
      FocusableRow {row = OnlyRow} -> id

navContext :: s -> Grid s r t -> NavContext s r t
navContext state navGrid =
  NavContext {state = initStateFocus state navGrid, navGrid}

focusedRow ::
  MonadState (NavContext s r t) m =>
  m (FocusedRow s r t)
focusedRow =
  use #navGrid <&> \ Grid {focus} -> focus

focusedTile ::
  MonadState (NavContext s r t) m =>
  m (Maybe (NavMeta s t))
focusedTile =
  focusedRow <&> \case
    FocusableRow {row = ActiveRow {focus}} -> Just focus
    FocusableRow {row = OnlyRow} -> Nothing

updateFocusedRow ::
  MonadState (NavContext s r t) m =>
  Bool ->
  m ()
updateFocusedRow new = do
  FocusableRow {meta = NavMeta {lens}} <- focusedRow
  #state . lens . #focused .= new

updateFocusedTile ::
  MonadState (NavContext s r t) m =>
  Bool ->
  m ()
updateFocusedTile new =
  focusedTile >>= traverse_ \ NavMeta {lens} -> #state . lens . #focused .= new

overFocusedRow ::
  MonadState (NavContext s r t) m =>
  (r -> r) ->
  m ()
overFocusedRow f = do
  FocusableRow {meta = NavMeta {lens}} <- focusedRow
  #state . lens . #state %= f

newtype OldFocus a =
  OldFocus a
  deriving stock (Eq, Show)

newtype NewFocus a =
  NewFocus a
  deriving stock (Eq, Show)

data NavHandlers grid meta focus elem result =
  NavHandlers {
    extract :: grid -> Maybe (meta, focus, Seq elem, Seq elem),
    inject :: grid -> meta -> focus -> Seq elem -> Seq elem -> grid,
    tryFocus :: OldFocus focus -> NewFocus elem -> Maybe focus,
    unfocus :: focus -> elem
  }

-- | Lift nav structures from inner state @s@ to outer state @s'@ by prepending a lens.
-- This allows screen-specific code to build nav grids rooted at the screen state,
-- then lift them to the full UI state.

liftNavMeta :: Traversal' s' s -> NavMeta s a -> NavMeta s' a
liftNavMeta l NavMeta {index, lens} = NavMeta {index, lens = l . lens}

liftTile :: Traversal' s' s -> Tile s t -> Tile s' t
liftTile l = \case
  TileUnfocusable -> TileUnfocusable
  TileFocusable meta -> TileFocusable (liftNavMeta l meta)

liftTiles :: Traversal' s' s -> Tiles s t -> Tiles s' t
liftTiles l Tiles {tiles} = Tiles {tiles = liftTile l <$> tiles}

liftActiveRow :: Traversal' s' s -> ActiveRow s t -> ActiveRow s' t
liftActiveRow l = \case
  OnlyRow -> OnlyRow
  ActiveRow {pre, focus, post} ->
    ActiveRow {
      pre = liftTiles l pre,
      focus = liftNavMeta l focus,
      post = liftTiles l post
    }

liftFocusableRow :: Traversal' s' s -> FocusableRow s r a -> FocusableRow s' r a
liftFocusableRow l FocusableRow {meta, row} =
  FocusableRow {meta = liftNavMeta l meta, row}

liftFocusedRow :: Traversal' s' s -> FocusedRow s r t -> FocusedRow s' r t
liftFocusedRow l FocusableRow {meta, row} =
  FocusableRow {meta = liftNavMeta l meta, row = liftActiveRow l row}

liftRow :: Traversal' s' s -> Row s r t -> Row s' r t
liftRow l = \case
  RowUnfocusable -> RowUnfocusable
  RowFocusable r -> RowFocusable (liftFocusableRow l r {row = liftTiles l r.row})

liftGrid :: Traversal' s' s -> Grid s r t -> Grid s' r t
liftGrid l Grid {pre, focus, post} =
  Grid {
    pre = liftRow l <$> pre,
    focus = liftFocusedRow l focus,
    post = liftRow l <$> post
  }

-- | Lift a NavContext from inner state to outer state.
-- Takes a lens from outer to inner, the outer state, and a grid rooted at inner state.
liftNavContext :: Traversal' s' s -> s' -> Grid s r t -> NavContext s' r t
liftNavContext l state grid =
  navContext state (liftGrid l grid)
