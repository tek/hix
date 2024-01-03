module Hix.Test.Managed.DiffTest where

import Data.These (These (These, This))
import Exon (exon)
import Hedgehog (evalMaybe, (===))
import Test.Tasty (TestTree, testGroup)

import Hix.Data.VersionBounds (versionBounds)
import Hix.Managed.Data.Diff (BoundsChange, VersionChange)
import Hix.Managed.Diff (boundsChange, versionChange)
import qualified Hix.Managed.EnvResult
import Hix.Managed.EnvResult (
  DepModification (DepUpdated),
  DepResult (DepResult),
  DepResultDetail (DepModified),
  depResult,
  )
import Hix.Managed.Handlers.Report.Prod (formatDepResult)
import Hix.Test.Utils (UnitTest, unitTest)

versionDiff1 :: VersionChange
versionDiff1 =
  versionChange (Just [1, 0]) (Just [1, 5])

boundsDiff1 :: BoundsChange
boundsDiff1 =
  boundsChange (versionBounds [1, 0] [2, 0]) (versionBounds [1, 5] [2, 0])

result1 :: Maybe DepResult
result1 =
  depResult "dep" versionDiff1 boundsDiff1

target1 :: DepResult
target1 =
  DepResult {
    package = "dep",
    version = [1, 5],
    bounds = versionBounds [1, 5] [2, 0],
    detail = DepModified (DepUpdated (These [1, 0] (This (Just [1, 0]))))
  }

test_boundsDiffDepResult :: UnitTest
test_boundsDiffDepResult = do
  res <- evalMaybe result1
  target1 === res
  formatted <- evalMaybe (formatDepResult res)
  ([exon|📦 [34mdep[0m|], [exon|[31m1.0[0m -> [32m1.5[0m|], [exon|↕ [[31m1.0[0m, 2.0] -> [[32m1.5[0m, 2.0]|]) === formatted

test_diff :: TestTree
test_diff =
  testGroup "Bounds and version diffs" [
    unitTest "BoundsChange to DepResult" test_boundsDiffDepResult
  ]
