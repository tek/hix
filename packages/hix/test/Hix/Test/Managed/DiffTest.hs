module Hix.Test.Managed.DiffTest where

import Data.Maybe (fromJust)
import Data.These (These (These, This))
import Exon (exon)
import Hedgehog (evalMaybe, (===))
import Test.Tasty (TestTree, testGroup)

import Hix.Data.VersionBounds (VersionBounds, fromLower, versionBounds)
import Hix.Managed.Data.Diff (BoundsChange, Change (..), Diff (..), VersionChange)
import Hix.Managed.Diff (boundsChange, boundsDiffDetail, updateBoundsChange, versionChange)
import qualified Hix.Managed.EnvResult
import Hix.Managed.EnvResult (
  BoundsModification (BoundsModification),
  DepModification (DepUpdated),
  DepResult (DepResult),
  DepResultDetail (DepModified),
  depResult,
  )
import Hix.Managed.Handlers.Report.Prod (formatDepResult)
import Hix.Test.Utils (UnitTest, unitTest)

versionChange1 :: VersionChange
versionChange1 =
  versionChange (Just [1, 0]) (Just [1, 5])

boundsChange1 :: BoundsChange
boundsChange1 =
  boundsChange (versionBounds [1, 0] [2, 0]) (versionBounds [1, 5] [2, 0])

result1 :: Maybe DepResult
result1 =
  depResult "dep" versionChange1 boundsChange1

target1 :: DepResult
target1 =
  DepResult {
    package = "dep",
    version = [1, 5],
    bounds = versionBounds [1, 5] [2, 0],
    detail = DepModified (DepUpdated (These [1, 0] (BoundsModification (This (Just [1, 0])))))
  }

test_boundsDiffDepResult :: UnitTest
test_boundsDiffDepResult = do
  res <- evalMaybe result1
  target1 === res
  formatted <- evalMaybe (formatDepResult res)
  ([exon|📦 [34mdep[0m|], [exon|[31m1.0[0m -> [32m1.5[0m|], [exon|↕ [[31m1.0[0m, 2.0] -> [[32m1.5[0m, 2.0]|]) === formatted

bounds2 :: VersionBounds
bounds2 = versionBounds [1, 0] [2, 0]

boundsChange2 :: BoundsChange
boundsChange2 = Unchanged (Just bounds2)

updatedBoundsChange2 :: BoundsChange
updatedBoundsChange2 =
  updateBoundsChange (fromLower [3, 0]) boundsChange2

targetBounds2 :: VersionBounds
targetBounds2 = versionBounds [3, 0] [3, 1]

targetBoundsChange2 :: BoundsChange
targetBoundsChange2 =
  Changed DiffChanged {
    original = bounds2,
    new = targetBounds2,
    detail = fromJust (boundsDiffDetail bounds2 targetBounds2)
  }

test_diffUpdateOneEndWithoutOverlap :: UnitTest
test_diffUpdateOneEndWithoutOverlap =
  targetBoundsChange2 === updatedBoundsChange2

test_diff :: TestTree
test_diff =
  testGroup "Bounds and version diffs" [
    unitTest "BoundsChange to DepResult" test_boundsDiffDepResult,
    unitTest "update one bound with no overlap with the previous bounds" test_diffUpdateOneEndWithoutOverlap
  ]
