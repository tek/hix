module Hix.Test.VersionTest where

import Distribution.Version (earlierVersion, intersectVersionRanges, laterVersion, orLaterVersion, unionVersionRanges)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)

import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major))
import Hix.Test.Utils (UnitTest, unitTest)
import Hix.Version (exclusiveUpperVersion, lastMajorBefore, lowerVersion, majorsBefore, versionsFrom)

test_versionList :: UnitTest
test_versionList =
  intersectVersionRanges (orLaterVersion [1, 4]) (earlierVersion [2, 3]) === [[1, 4], [2, 3]]

test_rangeBounds :: UnitTest
test_rangeBounds = do
  Just [1, 1] === lowerVersion range1
  Just [2, 3] === exclusiveUpperVersion range2
  where
    range1 = intersectVersionRanges (laterVersion [0, 4]) (unionVersionRanges [[1, 1], [1, 3]] range2)
    range2 = [[2, 2], [2, 3]]

test_majors :: UnitTest
test_majors = do
  [[2, 2]] === lastMajorBefore 2 3 versions
  target1 === majorsBefore 2 3 versions
  target2 === versionsFrom [2, 1, 2] versions
  drop 1 target2 === versionsFrom [2, 1, 3] versions
  where
    target1 =
      [
        Major {prefix = [2, 1], versions = [[2, 1, 1], [2, 1, 2]]},
        Major {prefix = [2, 2], versions = [[2, 2]]}
      ]
    target2 =
      [
        Major {prefix = [2, 1], versions = [[2, 1, 2]]},
        Major {prefix = [2, 2], versions = [[2, 2]]},
        Major {prefix = [2, 3], versions = [[2, 3]]}
      ]

    versions = [[2, 1, 1], [2, 1, 2], [2, 2], [2, 3]]

test_version :: TestTree
test_version =
  testGroup "version" [
    unitTest "fromList" test_versionList,
    unitTest "bounds" test_rangeBounds,
    unitTest "majors" test_majors
  ]
