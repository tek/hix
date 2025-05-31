module Hix.Pretty where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Path (Path, SomeBase (Abs, Rel), toFilePath)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (Doc, comma, hang, hcat, punctuate, sep, text, vcat, (<+>))

import Hix.Class.Map (NMap, nGet)
import Hix.Data.PathSpec (PathSpec (PathConcrete, PathUser))

showP ::
  ∀ b a .
  Pretty a =>
  IsString b =>
  a ->
  b
showP = show . pretty

showPM ::
  ∀ b a .
  Pretty a =>
  IsString b =>
  Maybe a ->
  b
showPM = show . fmap pretty

prettyL ::
  ∀ t a .
  Pretty a =>
  Foldable t =>
  t a ->
  Doc
prettyL = sep . punctuate comma . fmap pretty . toList

prettyL1 ::
  ∀ t a .
  Pretty a =>
  Foldable t =>
  t a ->
  Doc
prettyL1 = hcat . punctuate comma . fmap pretty . toList

showPL ::
  ∀ t b a .
  Pretty a =>
  Foldable t =>
  IsString b =>
  t a ->
  b
showPL = show . prettyL

prettyText :: Text -> Doc
prettyText = text . toString

prettyNt :: Coercible a Text => a -> Doc
prettyNt = prettyText . coerce

class HPretty a where
  hpretty :: a -> Doc
  hprettyField :: a -> Maybe Doc
  hprettyField = Just . hpretty

instance {-# overlappable #-} Pretty a => HPretty a where
  hpretty = pretty

instance HPretty String where
  hpretty = text

instance HPretty Text where
  hpretty = prettyText

instance HPretty Int where
  hpretty = show

instance HPretty Word where
  hpretty = show

instance HPretty Word16 where
  hpretty = show

instance HPretty (Path b t) where
  hpretty = hpretty . toFilePath

instance HPretty a => HPretty (Maybe a) where
  hpretty = foldMap hpretty
  hprettyField = fmap hpretty

instance (
    HPretty a,
    HPretty b
  ) => HPretty (Either a b) where
    hpretty = either hpretty hpretty

instance {-# overlappable #-} HPretty a => HPretty [a] where
  hpretty = showPL . fmap hpretty

instance HPretty a => HPretty (NonEmpty a) where
  hpretty = showPL . fmap hpretty

instance HPretty a => HPretty (Set a) where
  hpretty = showPL . fmap hpretty . toList

prettyFieldsKeys :: [(Doc, Maybe Doc)] -> [Doc]
prettyFieldsKeys =
  fmap (maybe mempty render . sequence)
  where
    render (desc, a) = desc PrettyPrint.<> ":" <+> pretty a

prettyFields :: [(Text, Maybe Doc)] -> [Doc]
prettyFields =
  prettyFieldsKeys . fmap (first prettyText)

prettyFieldsV :: [(Text, Maybe Doc)] -> Doc
prettyFieldsV = vcat . prettyFields

instance (Ord k, HPretty k, HPretty v) => HPretty (Map k v) where
  hpretty = hnPretty

prettyTuple :: [Doc] -> Doc
prettyTuple elems =
  prettyFieldsV [(show @_ @Word i, Just e) | (i, e) <- zip [1..] elems]

instance (
    HPretty a,
    HPretty b
  ) => HPretty (a, b) where
    hpretty (a, b) = prettyTuple [hpretty a, hpretty b]

instance (
    HPretty a,
    HPretty b,
    HPretty c
  ) => HPretty (a, b, c) where
    hpretty (a, b, c) = prettyTuple [hpretty a, hpretty b, hpretty c]

instance (
    HPretty a,
    HPretty b,
    HPretty c,
    HPretty d
  ) => HPretty (a, b, c, d) where
    hpretty (a, b, c, d) = prettyTuple [hpretty a, hpretty b, hpretty c, hpretty d]

instance HPretty (SomeBase t) where
  hpretty (Abs path) = hpretty path
  hpretty (Rel path) = hpretty path

instance HPretty (PathSpec t) where
  hpretty (PathConcrete path) = hpretty path
  hpretty (PathUser path) = hpretty path

prettyMapHead :: Text -> Doc -> Doc
prettyMapHead desc = hang (prettyText desc) 2

prettyMap :: Text -> [(Text, Maybe Doc)] -> Doc
prettyMap desc = prettyMapHead desc . prettyFieldsV

field :: HPretty a => Text -> a -> (Text, Maybe Doc)
field desc value = (desc, hprettyField value)

fieldWith :: Text -> Maybe a -> (a -> Doc) -> (Text, Maybe Doc)
fieldWith desc value render = (desc, render <$> value)

fieldWithOr :: Text -> Text -> Maybe a -> (a -> Doc) -> (Text, Maybe Doc)
fieldWithOr desc alt value render =
  (desc, Just (maybe (hpretty alt) render value))

fieldOr :: HPretty a => Text -> Text -> Maybe a -> (Text, Maybe Doc)
fieldOr desc alt value =
  fieldWithOr desc alt value hpretty

prettyV ::
  HPretty a =>
  Foldable t =>
  t a ->
  Doc
prettyV =
  vcat . fmap hpretty . toList

hnPrettyWith ::
  HPretty k =>
  NMap map k v sort =>
  (v -> Doc) ->
  map ->
  Doc
hnPrettyWith prettyVal (nGet -> m) =
  sep (punctuate comma (assoc <$> Map.toList m))
  where
    assoc (k, v) = hpretty k <+> "->" <+> prettyVal v

hnPretty ::
  HPretty k =>
  HPretty v =>
  NMap map k v sort =>
  map ->
  Doc
hnPretty =
  hnPrettyWith hpretty

showHP ::
  HPretty a =>
  IsString b =>
  a ->
  b
showHP =
  show . hpretty
