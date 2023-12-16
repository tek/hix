module Hix.Class.Map where

import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (dropMissing, mapMissing, preserveMissing, zipWithMatched)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (Doc, comma, hang, punctuate, sep, vcat, (<+>))

import Hix.Data.Monad (M)
import Hix.Monad (noteFatal)

data LookupMonoid
data LookupMaybe
data LookupFatal

type NtMap :: Type -> Type -> Type -> Type -> Constraint
class (Ord k, Coercible map (Map k v)) => NtMap map k v sort | map -> k v sort where
  ntMap :: map -> Map k v
  ntMap = coerce

instance Ord k => NtMap (Map k v) k v LookupMaybe where

type Lookup :: Type -> Type -> Type -> Type -> Constraint
class Lookup sort k v l | sort v -> l where
  lookup :: k -> Maybe v -> l

instance Monoid v => Lookup LookupMonoid k v v where
  lookup _ = fold

instance Lookup LookupMaybe k v (Maybe v) where
  lookup _ = id

instance Show k => Lookup LookupFatal k v (Text -> M v) where
  lookup k v thing = noteFatal [exon|No such #{thing}: #{show k}|] v

(!!) ::
  ∀ map k v sort l .
  NtMap map k v sort =>
  Lookup sort k v l =>
  map ->
  k ->
  l
(!!) m k = lookup @sort @k @v @l k (coerce m !? k)

infixl !!

ntInsert ::
  ∀ map k v sort .
  NtMap map k v sort =>
  k ->
  v ->
  map ->
  map
ntInsert k v m =
  coerce (Map.insert k v (ntMap m))

ntUpdate ::
  ∀ map k v sort l .
  NtMap map k v sort =>
  Lookup sort k v l =>
  k ->
  map ->
  (l -> v) ->
  map
ntUpdate k m f =
  ntInsert k (f (m !! k)) m

ntUpdating ::
  ∀ map k v sort l .
  NtMap map k v sort =>
  Lookup sort k v l =>
  k ->
  (l -> v) ->
  map ->
  map
ntUpdating k =
  flip (ntUpdate k)

via ::
  NtMap map k v sort =>
  (Map k v -> Map k v) ->
  map ->
  map
via =
  coerce

convert ::
  NtMap map1 k v1 sort1 =>
  NtMap map2 k v2 sort2 =>
  (v1 -> v2) ->
  map1 ->
  map2
convert f =
  coerce . fmap f . ntMap

convert1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NtMap map1 k map1' s1 =>
  NtMap map2 k map2' s2 =>
  NtMap map1' k' v1 s1' =>
  NtMap map2' k' v2 s2' =>
  (v1 -> v2) ->
  map1 ->
  map2
convert1 =
  convert . convert

convertWithKey ::
  NtMap map1 k1 v1 sort1 =>
  NtMap map2 k2 v2 sort2 =>
  (k1 -> v1 -> (k2, v2)) ->
  map1 ->
  map2
convertWithKey f =
  coerce .
  Map.fromList .
  fmap (uncurry f) .
  Map.toList .
  ntMap

convertMaybe ::
  NtMap map1 k v1 sort1 =>
  NtMap map2 k v2 sort2 =>
  (v1 -> Maybe v2) ->
  map1 ->
  map2
convertMaybe f =
  coerce . Map.mapMaybeWithKey (const f) . ntMap

convertMaybe1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NtMap map1 k map1' s1 =>
  NtMap map2 k map2' s2 =>
  NtMap map1' k' v1 s1' =>
  NtMap map2' k' v2 s2' =>
  (v1 -> Maybe v2) ->
  map1 ->
  map2
convertMaybe1 =
  convert . convertMaybe

convertWithKeyMaybe ::
  NtMap map1 k1 v1 sort1 =>
  NtMap map2 k2 v2 sort2 =>
  (k1 -> v1 -> Maybe (k2, v2)) ->
  map1 ->
  map2
convertWithKeyMaybe f =
  coerce .
  Map.fromList .
  mapMaybe (uncurry f) .
  Map.toList .
  ntMap

ntPrettyWith ::
  Pretty k =>
  NtMap map k v sort =>
  (v -> Doc) ->
  map ->
  Doc
ntPrettyWith prettyV (ntMap -> m) =
  sep (punctuate comma (assoc <$> Map.toList m))
  where
    assoc (k, v) = pretty k <+> "->" <+> prettyV v

ntPretty ::
  Pretty k =>
  Pretty v =>
  NtMap map k v sort =>
  map ->
  Doc
ntPretty =
  ntPrettyWith pretty

ntPretty1 ::
  Pretty k =>
  Pretty v =>
  NtMap map k v sort =>
  map ->
  Doc
ntPretty1 (ntMap -> m) =
  vcat (assoc <$> Map.toList m)
  where
    assoc (k, v) = hang (pretty k PrettyPrint.<> ":") 2 (pretty v)

-- | For each key that is present in both maps, replace the value by the result of the combining function.
-- For everything else, leave @map2@ unchanged.
ntAmend ::
  ∀ map1 map2 k v1 v2 s1 s2 .
  NtMap map1 k v1 s1 =>
  NtMap map2 k v2 s2 =>
  (v1 -> v2 -> v2) ->
  map1 ->
  map2 ->
  map2
ntAmend matched map1 map2 =
  via (Map.merge dropMissing preserveMissing (zipWithMatched (const matched)) (ntMap map1)) map2

-- | For each key path that is present in both submaps, replace the value by the result of the combining function.
-- For everything else, leave @map2@ unchanged.
ntAmend1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NtMap map1 k map1' s1 =>
  NtMap map2 k map2' s2 =>
  NtMap map1' k' v1 s1' =>
  NtMap map2' k' v2 s2' =>
  (v1 -> v2 -> v2) ->
  map1 ->
  map2 ->
  map2
ntAmend1 matched map1 =
  via (Map.merge dropMissing preserveMissing (zipWithMatched matched1) (ntMap map1))
  where
    matched1 _ map1' map2' = ntAmend matched map1' map2'

-- | For each key that is present in both maps, replace the value by the result of the combining function.
-- For each key that is present in @map1@, insert the result of the conversion function.
-- Keep keys only present in @map2@.
ntPad ::
  ∀ map1 map2 k v1 v2 s1 s2 .
  NtMap map1 k v1 s1 =>
  NtMap map2 k v2 s2 =>
  (v1 -> v2 -> v2) ->
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
ntPad matched conv map1 map2 =
  via (Map.merge (mapMissing (const conv)) preserveMissing (zipWithMatched (const matched)) (ntMap map1)) map2

-- | For each key path that is present in both submaps, replace the value by the result of the combining function.
-- For each key and key path that is present only in @map1@ and each submap, insert the result of the conversion
-- function.
-- Keep keys only present in @map2@ and each submap.
ntPad1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NtMap map1 k map1' s1 =>
  NtMap map2 k map2' s2 =>
  NtMap map1' k' v1 s1' =>
  NtMap map2' k' v2 s2' =>
  (v1 -> v2 -> v2) ->
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
ntPad1 matched conv map1 map2 =
  via (Map.merge (mapMissing missing) preserveMissing (zipWithMatched matched1) (ntMap map1)) map2
  where
    missing _ = convert conv
    matched1 _ map1' map2' = ntPad matched conv map1' map2'

-- | For each key that is present only in @map1@, insert the result of the conversion function.
-- For everything else, leave @map2@ unchanged.
ntPadKeep ::
  ∀ map1 map2 k v1 v2 s1 s2 .
  NtMap map1 k v1 s1 =>
  NtMap map2 k v2 s2 =>
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
ntPadKeep =
  ntPad \ _ v2 -> v2

-- | For each key and key path that is present only in @map1@ and each submap, insert the result of the conversion
-- function.
-- For everything else, leave @map2@ and its submaps unchanged.
ntPadKeep1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NtMap map1 k map1' s1 =>
  NtMap map2 k map2' s2 =>
  NtMap map1' k' v1 s1' =>
  NtMap map2' k' v2 s2' =>
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
ntPadKeep1 =
  ntPad1 \ _ v2 -> v2

ntFromList ::
  NtMap map k v sort =>
  [(k, v)] ->
  map
ntFromList =
  coerce . Map.fromList

ntList ::
  NtMap map k v sort =>
  map ->
  [(k, v)]
ntList =
  Map.toList . coerce

ntTo ::
  NtMap map k v sort =>
  map ->
  (k -> v -> a) ->
  [a]
ntTo m f =
  fmap (uncurry f) (ntList m)

ntToWith ::
  NtMap map k v sort =>
  (k -> v -> a) ->
  map ->
  [a]
ntToWith = flip ntTo

ntConcat ::
  NtMap map k v sort =>
  map ->
  (k -> v -> [a]) ->
  [a]
ntConcat m f =
  uncurry f =<< ntList m

ntConcatWith ::
  NtMap map k v sort =>
  (k -> v -> [a]) ->
  map ->
  [a]
ntConcatWith =
  flip ntConcat

ntTo1 ::
  ∀ map1 map2 k1 k2 v s1 s2 a .
  NtMap map1 k1 map2 s1 =>
  NtMap map2 k2 v s2 =>
  map1 ->
  (k1 -> k2 -> v -> a) ->
  [a]
ntTo1 m1 f =
  ntConcat m1 (ntToWith . f)

ntToWith1 ::
  ∀ map1 map2 k1 k2 v s1 s2 a .
  NtMap map1 k1 map2 s1 =>
  NtMap map2 k2 v s2 =>
  (k1 -> k2 -> v -> a) ->
  map1 ->
  [a]
ntToWith1 f =
  ntConcatWith (ntToWith . f)

ntFromKeys ::
  NtMap map k v sort =>
  [k] ->
  (k -> v) ->
  map
ntFromKeys keys f =
  ntFromList (keys <&> \ k -> (k, f k))

ntBy ::
  NtMap map k v sort =>
  [v] ->
  (v -> k) ->
  map
ntBy values f =
  ntFromList (values <&> \ v -> (f v, v))

ntForKeys ::
  Applicative m =>
  NtMap map k v sort =>
  [k] ->
  (k -> m v) ->
  m map
ntForKeys keys f =
  ntFromList <$> for keys \ k -> (k,) <$> f k

ntFor ::
  Applicative m =>
  NtMap map k v sort =>
  [v] ->
  (v -> m k) ->
  m map
ntFor values f =
  ntFromList <$> for values \ v -> (,v) <$> f v

ntElems1 ::
  ∀ map1 map2 k1 k2 v s1 s2 .
  NtMap map1 k1 map2 s1 =>
  NtMap map2 k2 v s2 =>
  map1 ->
  [v]
ntElems1 =
  Map.elems . ntMap <=< Map.elems . ntMap

ntFlatten ::
  Monoid map2 =>
  NtMap map1 k1 map2 LookupMonoid =>
  map1 ->
  map2
ntFlatten =
  mconcat . Map.elems . ntMap

ntKeys ::
  NtMap map k v sort =>
  map ->
  [k]
ntKeys =
  Map.keys . ntMap
