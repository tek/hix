module Hix.Managed.Report where

import Exon (exon)

plural ::
  Eq a =>
  Num a =>
  a ->
  Text
plural num | num == 1 = ""
           | otherwise = "s"

describeIterations :: Natural -> Text
describeIterations iterations =
  [exon|#{show iterations} iteration#{plural iterations}|]
