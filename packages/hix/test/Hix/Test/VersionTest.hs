module Hix.Test.VersionTest where

import Distribution.Version (earlierVersion, intersectVersionRanges, laterVersion, orLaterVersion, unionVersionRanges)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)

import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major))
import qualified Hix.Managed.Data.VersionIncrement as VersionIncrement
import Hix.Managed.VersionIncrement (incrementVersion)
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

test_increment :: UnitTest
test_increment = do
  "3.0.0.0" === incrementVersion "2.3.4.5" VersionIncrement.Supermajor
  "1" === incrementVersion "0" VersionIncrement.Supermajor
  "2.3.0" === incrementVersion "2.2.5" VersionIncrement.Major
  "0.1" === incrementVersion "0" VersionIncrement.Major
  "2.3.6" === incrementVersion "2.3.5" VersionIncrement.Minor
  "0.0.1" === incrementVersion "0" VersionIncrement.Minor
  "2.3.0.1" === incrementVersion "2.3" VersionIncrement.Patch
  "2.3.4.6" === incrementVersion "2.3.4.5" VersionIncrement.Patch

test_version :: TestTree
test_version =
  testGroup "version" [
    unitTest "fromList" test_versionList,
    unitTest "bounds" test_rangeBounds,
    unitTest "majors" test_majors,
    unitTest "increment" test_increment
  ]
