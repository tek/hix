module Hix.Test.BoundsTest where

import Distribution.Version (orEarlierVersion, orLaterVersion)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Class.Map (nAmend1, nForKeys, nFromKeys, nMap1, nPadKeep1)
import Hix.Data.Bounds (Ranges)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep, mkDep, withVersion)
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Managed.Data.Packages (Deps, Packages)

packages :: [PackageName]
packages = ["dep1", "dep2", "dep3"]

targets :: [LocalPackage]
targets = ["target1", "target2", "target3"]

genBounds :: Gen Ranges
genBounds = do
  keys <- Gen.subsequence packages
  pure (nFromKeys keys (const dep))
  where
    dep = orEarlierVersion [2, 5]

genTargetBounds :: Gen (Packages Ranges)
genTargetBounds = do
  keys <- Gen.subsequence targets
  nForKeys keys (const genBounds)

genDeps :: Deps Dep
genDeps =
  nFromKeys packages \ k -> mkDep k (orLaterVersion [1, 5])

deps :: Packages (Deps Dep)
deps =
  nFromKeys targets (const genDeps)

prop_amendBounds :: Property
prop_amendBounds =
  property do
    bounds <- forAll genTargetBounds
    nPadKeep1 (.version) deps bounds === nMap1 (.version) (nAmend1 withVersion bounds deps)

test_bounds :: TestTree
test_bounds =
  testGroup "bounds" [
    testProperty "pad a b === amend b a (for padded a)" prop_amendBounds
  ]
