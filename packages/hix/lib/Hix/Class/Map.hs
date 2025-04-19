module Hix.Class.Map where

import qualified Data.Map.Merge.Strict as Map
import Data.Map.Merge.Strict (
  SimpleWhenMatched,
  SimpleWhenMissing,
  WhenMatched,
  WhenMissing,
  dropMissing,
  mapMissing,
  preserveMissing,
  zipWithMatched,
  )
import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (Doc, comma, hang, punctuate, sep, vcat, (<+>))

import Hix.Data.Monad (M)
import Hix.Monad (noteFatal)

data LookupMonoid
data LookupMaybe
data LookupFatal

type NMap :: Type -> Type -> Type -> Type -> Constraint
class (Ord k, Coercible map (Map k v)) => NMap map k v sort | map -> k v sort where
  nGet :: map -> Map k v
  nGet = coerce

instance Ord k => NMap (Map k v) k v LookupMaybe where

lookupError ::
  ∀ k v .
  Show k =>
  Text ->
  k ->
  Maybe v ->
  M v
lookupError thing k =
  noteFatal [exon|No such #{thing}: #{show k}|]

nFatal ::
  ∀ map k v sort .
  Show k =>
  NMap map k v sort =>
  Text ->
  k ->
  map ->
  M v
nFatal thing k m =
  lookupError thing k (nGet m Map.!? k)

type NLookup :: Type -> Type -> Type -> Type -> Constraint
class NLookup sort k v l | sort v -> l where
  nLookup :: k -> Maybe v -> l

instance Monoid v => NLookup LookupMonoid k v v where
  nLookup _ = fold

instance NLookup LookupMaybe k v (Maybe v) where
  nLookup _ = id

(!?) ::
  ∀ map k v sort .
  NMap map k v sort =>
  map ->
  k ->
  Maybe v
(!?) m k = nGet m Map.!? k

infixl !?

(!!) ::
  ∀ map k v sort l .
  NMap map k v sort =>
  NLookup sort k v l =>
  map ->
  k ->
  l
(!!) m k = nLookup @sort @k @v @l k (m !? k)

infixl !!

nInsert ::
  ∀ map k v sort .
  NMap map k v sort =>
  k ->
  v ->
  map ->
  map
nInsert k v m =
  coerce (Map.insert k v (nGet m))

nUpdate ::
  ∀ map k v sort l .
  NMap map k v sort =>
  NLookup sort k v l =>
  k ->
  map ->
  (l -> v) ->
  map
nUpdate k m f =
  nInsert k (f (m !! k)) m

nUpdateWith ::
  ∀ map k v sort l .
  NMap map k v sort =>
  NLookup sort k v l =>
  k ->
  (l -> v) ->
  map ->
  map
nUpdateWith k =
  flip (nUpdate k)

nAdjust ::
  ∀ map k v sort .
  NMap map k v sort =>
  k ->
  map ->
  (v -> v) ->
  map
nAdjust k m f =
  nVia (Map.adjust f k) m

nVia ::
  NMap map1 k1 v1 s1 =>
  NMap map2 k2 v2 s2 =>
  (Map k1 v1 -> Map k2 v2) ->
  map1 ->
  map2
nVia =
  coerce

nViaA ::
  Applicative m =>
  NMap map1 k1 v1 s1 =>
  NMap map2 k2 v2 s2 =>
  (Map k1 v1 -> m (Map k2 v2)) ->
  map1 ->
  m map2
nViaA f m =
  coerce <$> f (coerce m)

nMapKeys ::
  NMap map1 k1 v sort1 =>
  NMap map2 k2 v sort2 =>
  (k1 -> k2) ->
  map1 ->
  map2
nMapKeys f =
  nVia (Map.mapKeys f)

nMap ::
  NMap map1 k v1 sort1 =>
  NMap map2 k v2 sort2 =>
  (v1 -> v2) ->
  map1 ->
  map2
nMap f =
  coerce . fmap f . nGet

nMap1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NMap map1 k map1' s1 =>
  NMap map2 k map2' s2 =>
  NMap map1' k' v1 s1' =>
  NMap map2' k' v2 s2' =>
  (v1 -> v2) ->
  map1 ->
  map2
nMap1 =
  nMap . nMap

nMapWithKey ::
  NMap map1 k v1 sort1 =>
  NMap map2 k v2 sort2 =>
  (k -> v1 -> v2) ->
  map1 ->
  map2
nMapWithKey f =
  coerce . Map.mapWithKey f . nGet

nMapWithKey1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NMap map1 k map1' s1 =>
  NMap map2 k map2' s2 =>
  NMap map1' k' v1 s1' =>
  NMap map2' k' v2 s2' =>
  (k -> k' -> v1 -> v2) ->
  map1 ->
  map2
nMapWithKey1 f =
  nMapWithKey \ k -> nMapWithKey (f k)

nOver ::
  NMap map1 k v1 sort1 =>
  NMap map2 k v2 sort2 =>
  map1 ->
  (v1 -> v2) ->
  map2
nOver =
  flip nMap

nViaList ::
  NMap map1 k1 v1 sort1 =>
  NMap map2 k2 v2 sort2 =>
  ([(k1, v1)] -> [(k2, v2)]) ->
  map1 ->
  map2
nViaList f =
  coerce .
  Map.fromList .
  f .
  Map.toList .
  nGet

nTransform ::
  NMap map1 k1 v1 sort1 =>
  NMap map2 k2 v2 sort2 =>
  (k1 -> v1 -> (k2, v2)) ->
  map1 ->
  map2
nTransform f =
  nViaList (fmap (uncurry f))

nTransformMaybe ::
  NMap map1 k1 v1 sort1 =>
  NMap map2 k2 v2 sort2 =>
  (k1 -> v1 -> Maybe (k2, v2)) ->
  map1 ->
  map2
nTransformMaybe f =
  nViaList (mapMaybe (uncurry f))

nTransformMulti ::
  NMap map1 k1 v1 sort1 =>
  NMap map2 k2 v2 sort2 =>
  (k1 -> v1 -> [(k2, v2)]) ->
  map1 ->
  map2
nTransformMulti f =
  nViaList (>>= (uncurry f))

nMapMaybe ::
  NMap map1 k v1 sort1 =>
  NMap map2 k v2 sort2 =>
  (v1 -> Maybe v2) ->
  map1 ->
  map2
nMapMaybe f =
  coerce . Map.mapMaybeWithKey (const f) . nGet

nMapMaybe1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NMap map1 k map1' s1 =>
  NMap map2 k map2' s2 =>
  NMap map1' k' v1 s1' =>
  NMap map2' k' v2 s2' =>
  (v1 -> Maybe v2) ->
  map1 ->
  map2
nMapMaybe1 =
  nMap . nMapMaybe

nMapMaybeWithKey ::
  NMap map1 k v1 sort1 =>
  NMap map2 k v2 sort2 =>
  (k -> v1 -> Maybe v2) ->
  map1 ->
  map2
nMapMaybeWithKey f =
  coerce .
  Map.mapMaybeWithKey f .
  nGet

nMapMaybeWithKey1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NMap map1 k map1' s1 =>
  NMap map2 k map2' s2 =>
  NMap map1' k' v1 s1' =>
  NMap map2' k' v2 s2' =>
  (k' -> v1 -> Maybe v2) ->
  map1 ->
  map2
nMapMaybeWithKey1 =
  nMap . nMapMaybeWithKey

nCatMaybes ::
  NMap map1 k (Maybe v) sort1 =>
  NMap map2 k v sort2 =>
  map1 ->
  map2
nCatMaybes =
  nMapMaybe id

nFilter ::
  ∀ map k v sort .
  NMap map k v sort =>
  (v -> Bool) ->
  map ->
  map
nFilter =
  nVia . Map.filter

nFilter1 ::
  ∀ map map' k k' v s s' .
  NMap map k map' s =>
  NMap map' k' v s' =>
  (v -> Bool) ->
  map ->
  map
nFilter1 =
  nMap . nFilter

nPrettyWith ::
  Pretty k =>
  NMap map k v sort =>
  (v -> Doc) ->
  map ->
  Doc
nPrettyWith prettyV (nGet -> m) =
  sep (punctuate comma (assoc <$> Map.toList m))
  where
    assoc (k, v) = pretty k <+> "->" <+> prettyV v

nPretty ::
  Pretty k =>
  Pretty v =>
  NMap map k v sort =>
  map ->
  Doc
nPretty =
  nPrettyWith pretty

nPretty1 ::
  Pretty k =>
  Pretty v =>
  NMap map k v sort =>
  map ->
  Doc
nPretty1 (nGet -> m) =
  vcat (assoc <$> Map.toList m)
  where
    assoc (k, v) = hang (pretty k PrettyPrint.<> ":") 2 (pretty v)

nMergeA ::
  ∀ map1 map2 map3 k v1 v2 v3 s1 s2 s3 m .
  Applicative m =>
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  NMap map3 k v3 s3 =>
  WhenMissing m k v1 v3 ->
  WhenMissing m k v2 v3 ->
  WhenMatched m k v1 v2 v3 ->
  map1 ->
  map2 ->
  m map3
nMergeA missing1 missing2 matched map1 map2 =
  nViaA (Map.mergeA missing1 missing2 matched (nGet map1)) map2

nMerge ::
  ∀ map1 map2 map3 k v1 v2 v3 s1 s2 s3 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  NMap map3 k v3 s3 =>
  SimpleWhenMissing k v1 v3 ->
  SimpleWhenMissing k v2 v3 ->
  SimpleWhenMatched k v1 v2 v3 ->
  map1 ->
  map2 ->
  map3
nMerge missing1 missing2 matched map1 map2 =
  nVia (Map.merge missing1 missing2 matched (nGet map1)) map2

nAmendWithKey ::
  ∀ map1 map2 k v1 v2 s1 s2 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  (k -> v1 -> v2 -> v2) ->
  map1 ->
  map2 ->
  map2
nAmendWithKey matched =
  nMerge dropMissing preserveMissing (zipWithMatched matched)

-- | For each key that is present in both maps, replace the value by the result of the combining function.
-- For everything else, leave @map2@ unchanged.
nAmend ::
  ∀ map1 map2 k v1 v2 s1 s2 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  (v1 -> v2 -> v2) ->
  map1 ->
  map2 ->
  map2
nAmend matched =
  nAmendWithKey (const matched)

-- | For each key path that is present in both submaps, replace the value by the result of the combining function.
-- For everything else, leave @map2@ unchanged.
nAmend1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NMap map1 k map1' s1 =>
  NMap map2 k map2' s2 =>
  NMap map1' k' v1 s1' =>
  NMap map2' k' v2 s2' =>
  (v1 -> v2 -> v2) ->
  map1 ->
  map2 ->
  map2
nAmend1 matched =
  nMerge dropMissing preserveMissing (zipWithMatched matched1)
  where
    matched1 _ map1' map2' = nAmend matched map1' map2'

-- | For each key that is present in both maps, replace the value by the result of the combining function.
-- For each key that is present in @map1@, insert the result of the conversion function.
-- Keep keys only present in @map2@.
nPad ::
  ∀ map1 map2 k v1 v2 s1 s2 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  (v1 -> v2 -> v2) ->
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
nPad matched conv =
  nMerge (mapMissing (const conv)) preserveMissing (zipWithMatched (const matched))

-- | For each key path that is present in both submaps, replace the value by the result of the combining function.
-- For each key and key path that is present only in @map1@ and each submap, insert the result of the conversion
-- function.
-- Keep keys only present in @map2@ and each submap.
nPad1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NMap map1 k map1' s1 =>
  NMap map2 k map2' s2 =>
  NMap map1' k' v1 s1' =>
  NMap map2' k' v2 s2' =>
  (v1 -> v2 -> v2) ->
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
nPad1 matched conv =
  nMerge (mapMissing missing) preserveMissing (zipWithMatched matched1)
  where
    missing _ = nMap conv
    matched1 _ = nPad matched conv

-- | For each key that is present only in @map1@, insert the result of the conversion function.
-- For everything else, leave @map2@ unchanged.
nPadKeep ::
  ∀ map1 map2 k v1 v2 s1 s2 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
nPadKeep =
  nPad \ _ v2 -> v2

-- | For each key and key path that is present only in @map1@ and each submap, insert the result of the conversion
-- function.
-- For everything else, leave @map2@ and its submaps unchanged.
nPadKeep1 ::
  ∀ map1 map2 map1' map2' k k' v1 v2 s1 s2 s1' s2' .
  NMap map1 k map1' s1 =>
  NMap map2 k map2' s2 =>
  NMap map1' k' v1 s1' =>
  NMap map2' k' v2 s2' =>
  (v1 -> v2) ->
  map1 ->
  map2 ->
  map2
nPadKeep1 =
  nPad1 \ _ v2 -> v2

nZipWithKeyR ::
  ∀ map1 map2 map3 k v1 v2 v3 s1 s2 s3 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  NMap map3 k v3 s3 =>
  (k -> Maybe v1 -> v2 -> v3) ->
  map1 ->
  map2 ->
  map3
nZipWithKeyR f =
  nMerge dropMissing (mapMissing missing) (zipWithMatched matched)
  where
    missing k = f k Nothing
    matched k v = f k (Just v)

nZipR ::
  ∀ map1 map2 map3 k v1 v2 v3 s1 s2 s3 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  NMap map3 k v3 s3 =>
  (Maybe v1 -> v2 -> v3) ->
  map1 ->
  map2 ->
  map3
nZipR f = nZipWithKeyR (const f)

nZipWithKey ::
  ∀ map1 map2 map3 k v1 v2 v3 s1 s2 s3 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  NMap map3 k v3 s3 =>
  (k -> v1 -> v2 -> v3) ->
  map1 ->
  map2 ->
  map3
nZipWithKey f =
  nMerge dropMissing dropMissing (zipWithMatched f)

nZip ::
  ∀ map1 map2 map3 k v1 v2 v3 s1 s2 s3 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  NMap map3 k v3 s3 =>
  (v1 -> v2 -> v3) ->
  map1 ->
  map2 ->
  map3
nZip f = nZipWithKey (const f)

nPartitionWithKey ::
  ∀ map1 map2 map3 k1 k2 k3 v1 v2 v3 s1 s2 s3 .
  NMap map1 k1 v1 s1 =>
  NMap map2 k2 v2 s2 =>
  NMap map3 k3 v3 s3 =>
  (k1 -> v1 -> Either (k2, v2) (k3, v3)) ->
  map1 ->
  (map2, map3)
nPartitionWithKey f =
  coerce .
  Map.foldl' step (Map.empty, Map.empty) .
  Map.mapWithKey f .
  nGet
  where
    step (l, r) = \case
      Left (k, v2) -> (Map.insert k v2 l, r)
      Right (k, v3) -> (l, Map.insert k v3 r)

nPartitionByKey ::
  ∀ map1 map2 map3 k1 k2 k3 v s1 s2 s3 .
  NMap map1 k1 v s1 =>
  NMap map2 k2 v s2 =>
  NMap map3 k3 v s3 =>
  (k1 -> Either k2 k3) ->
  map1 ->
  (map2, map3)
nPartitionByKey f =
  nPartitionWithKey \ k1 v -> bimap (,v) (,v) (f k1)

nPartition ::
  ∀ map1 map2 map3 k v1 v2 v3 s1 s2 s3 .
  NMap map1 k v1 s1 =>
  NMap map2 k v2 s2 =>
  NMap map3 k v3 s3 =>
  (v1 -> Either v2 v3) ->
  map1 ->
  (map2, map3)
nPartition f =
  nPartitionWithKey \ k v1 -> bimap (k,) (k,) (f v1)

nFromList ::
  NMap map k v sort =>
  [(k, v)] ->
  map
nFromList =
  coerce . Map.fromList

nList ::
  NMap map k v sort =>
  map ->
  [(k, v)]
nList =
  Map.toList . coerce

nTo ::
  NMap map k v sort =>
  map ->
  (k -> v -> a) ->
  [a]
nTo m f =
  fmap (uncurry f) (nList m)

nToWith ::
  NMap map k v sort =>
  (k -> v -> a) ->
  map ->
  [a]
nToWith = flip nTo

nConcat ::
  Monoid a =>
  NMap map k v sort =>
  map ->
  (k -> v -> a) ->
  a
nConcat m f =
  mconcat (uncurry f <$> nList m)

nConcatWith ::
  Monoid a =>
  NMap map k v sort =>
  (k -> v -> a) ->
  map ->
  a
nConcatWith =
  flip nConcat

nToMaybe ::
  NMap map k v sort =>
  map ->
  (k -> v -> Maybe a) ->
  [a]
nToMaybe m f =
  mapMaybe (uncurry f) (nList m)

nTo1 ::
  ∀ map1 map2 k1 k2 v s1 s2 a .
  NMap map1 k1 map2 s1 =>
  NMap map2 k2 v s2 =>
  map1 ->
  (k1 -> k2 -> v -> a) ->
  [a]
nTo1 m1 f =
  nConcat m1 (nToWith . f)

nToWith1 ::
  ∀ map1 map2 k1 k2 v s1 s2 a .
  NMap map1 k1 map2 s1 =>
  NMap map2 k2 v s2 =>
  (k1 -> k2 -> v -> a) ->
  map1 ->
  [a]
nToWith1 f =
  nConcatWith (nToWith . f)

nFromKeys ::
  Foldable t =>
  NMap map k v sort =>
  t k ->
  (k -> v) ->
  map
nFromKeys keys f =
  nFromList (toList keys <&> \ k -> (k, f k))

nFromKeysMaybe ::
  Foldable t =>
  NMap map k v sort =>
  t k ->
  (k -> Maybe v) ->
  map
nFromKeysMaybe keys f =
  nFromList (flip mapMaybe (toList keys) \ k -> (k,) <$> f k)

nBy ::
  Foldable t =>
  NMap map k v sort =>
  t v ->
  (v -> k) ->
  map
nBy values f =
  nFromList (toList values <&> \ v -> (f v, v))

nGen ::
  Foldable t =>
  NMap map k v sort =>
  t a ->
  (a -> (k, v)) ->
  map
nGen values f =
  nFromList (f <$> toList values)

nGenWith ::
  Foldable t =>
  NMap map k v sort =>
  (a -> (k, v)) ->
  t a ->
  map
nGenWith =
  flip nGen

nGenMaybe ::
  Foldable t =>
  NMap map k v sort =>
  t a ->
  (a -> Maybe (k, v)) ->
  map
nGenMaybe values f =
  nFromList (mapMaybe f (toList values))

nForKeys ::
  Applicative m =>
  NMap map k v sort =>
  [k] ->
  (k -> m v) ->
  m map
nForKeys keys f =
  nFromList <$> for keys \ k -> (k,) <$> f k

nFor ::
  Applicative m =>
  NMap map k v sort =>
  [v] ->
  (v -> m k) ->
  m map
nFor values f =
  nFromList <$> for values \ v -> (,v) <$> f v

nForAssoc ::
  Applicative m =>
  NMap map k v sort =>
  [a] ->
  (a -> m (k, v)) ->
  m map
nForAssoc seed f =
  nFromList <$> traverse f seed

nElems ::
  ∀ map k v s .
  NMap map k v s =>
  map ->
  [v]
nElems =
  Map.elems . nGet

nElems1 ::
  ∀ map1 map2 k1 k2 v s1 s2 .
  NMap map1 k1 map2 s1 =>
  NMap map2 k2 v s2 =>
  map1 ->
  [v]
nElems1 =
  nElems <=< nElems

nFlatten ::
  ∀ map1 map2 map3 k1 k2 v1 v2 s1 s2 .
  Monoid map2 =>
  NMap map1 k1 map2 LookupMonoid =>
  NMap map2 k2 v1 s1 =>
  NMap map3 k2 v2 s2 =>
  (v1 -> v2) ->
  map1 ->
  map3
nFlatten f =
  nMap f . mconcat . nElems

nKeys ::
  NMap map k v sort =>
  map ->
  [k]
nKeys =
  Map.keys . nGet

nKeysSet ::
  NMap map k v sort =>
  map ->
  Set k
nKeysSet =
  Map.keysSet . nGet

nRestrictKeys ::
  NMap map k v sort =>
  Set k ->
  map ->
  map
nRestrictKeys keys =
  nVia (`Map.restrictKeys` keys)

nWithoutKeys ::
  NMap map k v sort =>
  Set k ->
  map ->
  map
nWithoutKeys keys =
  nVia (`Map.withoutKeys` keys)

nMember ::
  NMap map k v sort =>
  k ->
  map ->
  Bool
nMember k =
  Map.member k . nGet

nNull ::
  NMap map k v sort =>
  map ->
  Bool
nNull =
  null . nGet
