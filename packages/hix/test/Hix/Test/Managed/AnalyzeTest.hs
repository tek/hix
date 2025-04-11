module Hix.Test.Managed.AnalyzeTest where

import Distribution.Parsec (eitherParsec)
import Distribution.Version (majorBoundVersion, orEarlierVersion)
import Hedgehog ((===))

import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep (..))
import Hix.Managed.Build.NixOutput.Analysis (CommaSeparatedDeps (..))
import Hix.Test.Utils (UnitTest)

target :: CommaSeparatedDeps
target =
  CommaSeparatedDeps (Dep.toCabal <$> [
    Dep {package = "aaa", version = orEarlierVersion [1]},
    Dep {package = "bbb", version = majorBoundVersion [2]},
    Dep {package = "ccc", version = [[3], [4]]}
  ])

test_parseCommaSeparatedDeps :: UnitTest
test_parseCommaSeparatedDeps = do
  Right target === eitherParsec "aaa <=1, bbb ^>=2, ccc ==3.* (garbage)"
