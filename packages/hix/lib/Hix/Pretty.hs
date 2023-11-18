module Hix.Pretty where

import Distribution.Pretty (Pretty, pretty)
import Text.PrettyPrint (Doc, comma, punctuate, sep)

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

prettyL :: Pretty a => [a] -> Doc
prettyL = sep . punctuate comma . fmap pretty

showPL ::
  ∀ b a .
  Pretty a =>
  IsString b =>
  [a] ->
  b
showPL = show . prettyL
