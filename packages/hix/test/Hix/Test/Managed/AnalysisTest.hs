module Hix.Test.Managed.AnalysisTest where

import Distribution.Parsec (eitherParsec)
import Distribution.Version (majorBoundVersion, orEarlierVersion)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)

import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep (..))
import Hix.Managed.Build.NixOutput.Analysis (CommaSeparatedDeps (..), UnknownDepMessage (..), sanitizeOutput)
import Hix.Test.Utils (UnitTest, unitTest)

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

target_unknownDepNixpkgs :: UnknownDepMessage
target_unknownDepNixpkgs =
  UnknownDepMessage "dep3"

test_unknownDepNixpkgs :: UnitTest
test_unknownDepNixpkgs =
  Right target_unknownDepNixpkgs === eitherParsec (toString (sanitizeOutput message))
  where
    message = "       \ESC[31;1merror:\ESC[0m function '\ESC[35;1manonymous lambda\ESC[0m' called without required" <>
              " argument '\ESC[35;1mdep3\ESC[0m'"

test_analysis :: TestTree
test_analysis =
  testGroup "nix output analysis" [
    unitTest "comma separated deps" test_parseCommaSeparatedDeps,
    unitTest "unknown dep nixpkgs" test_unknownDepNixpkgs
  ]
