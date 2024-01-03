module Hix.Pretty where

import Distribution.Pretty (Pretty, pretty)
import Text.PrettyPrint (Doc, comma, punctuate, sep, text)

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
