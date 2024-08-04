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

describeIterations :: Natural -> Text
describeIterations iterations =
  [exon|#{show iterations} iteration#{plural iterations}|]
