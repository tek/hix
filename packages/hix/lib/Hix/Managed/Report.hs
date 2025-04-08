module Hix.Managed.Report where

import Exon (exon)

pluralWith ::
  Eq a =>
  Num a =>
  Text ->
  a ->
  Text
pluralWith suffix num
  | num == 1 = ""
  | otherwise = suffix

plural ::
  Eq a =>
  Num a =>
  a ->
  Text
plural = pluralWith "s"

pluralLength :: Foldable t => t a -> Text
pluralLength ta = pluralWith "s" (length ta)

describeIterations :: Word -> Text
describeIterations iterations =
  [exon|#{show iterations} iteration#{plural iterations}|]
