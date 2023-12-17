module Hix.Test.BoundsTest where

import Distribution.Version (orEarlierVersion, orLaterVersion)
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Class.Map (ntAmend1, ntForKeys, ntFromKeys, ntPadKeep1)
import Hix.Data.Bounds (Bounds, TargetBounds)
import qualified Hix.Data.Dep
import Hix.Data.Dep (mainDep, withVersion)
import Hix.Data.Deps (RemoteDeps, TargetRemoteDeps)
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Deps (depsToTargetBounds)

packages :: [PackageName]
packages = ["dep1", "dep2", "dep3"]

targets :: [LocalPackage]
targets = ["target1", "target2", "target3"]

genBounds :: Gen Bounds
genBounds = do
  keys <- Gen.subsequence packages
  pure (ntFromKeys keys (const dep))
  where
    dep = orEarlierVersion [2, 5]

genTargetBounds :: Gen TargetBounds
genTargetBounds = do
  keys <- Gen.subsequence targets
  ntForKeys keys (const genBounds)

genDeps :: RemoteDeps
genDeps =
  ntFromKeys packages \ k -> mainDep k (orLaterVersion [1, 5])

deps :: TargetRemoteDeps
deps =
  ntFromKeys targets (const genDeps)

prop_amendBounds :: Property
prop_amendBounds =
  property do
    bounds <- forAll genTargetBounds
    ntPadKeep1 (.version) deps bounds === depsToTargetBounds (ntAmend1 withVersion bounds deps)

test_bounds :: TestTree
test_bounds =
  testGroup "bounds" [
    testProperty "pad a b === amend b a (for padded a)" prop_amendBounds
  ]
