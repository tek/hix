module Hix.Test.VersionTest where

import Data.List.Extra (dropEnd)
import Distribution.Version (
  Bound (ExclusiveBound, InclusiveBound),
  LowerBound (LowerBound),
  UpperBound (NoUpperBound, UpperBound),
  Version,
  VersionInterval (VersionInterval),
  VersionRange,
  asVersionIntervals,
  earlierVersion,
  intersectVersionRanges,
  laterVersion,
  orEarlierVersion,
  orLaterVersion,
  thisVersion,
  unionVersionRanges,
  version0,
  )
import Hedgehog (Gen, Property, classify, forAllWith, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major), range0)
import Hix.Pretty (showP)
import Hix.Test.Utils (UnitTest, unitTest)
import Hix.Version (
  amendLowerBound,
  lastMajorBefore,
  lowerBound,
  majorsBefore,
  rangeFromIntervals,
  requireUpperBound,
  setLowerBound,
  setUpperBound,
  upperBound,
  versionsFrom,
  )

test_versionList :: UnitTest
test_versionList =
  intersectVersionRanges (orLaterVersion [1, 4]) (earlierVersion [2, 3]) === [[1, 4], [2, 3]]

test_rangeBounds :: UnitTest
test_rangeBounds = do
  Just [1, 1] === lowerBound range1
  Just [2, 3] === upperBound range2
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

test_setLowerBound :: UnitTest
test_setLowerBound = do
  range1 === setLowerBound [2, 2] range1
  range2 === setLowerBound [2, 1] range1
  range3 === setLowerBound [2, 1] (thisVersion [2, 3])
  [[2, 1]] === setLowerBound [2, 1] (laterVersion [2, 2])
  range2 === setLowerBound [2, 1] (earlierVersion [2, 3])
  [[2, 1]] === setLowerBound [2, 1] range0
  [[2, 1], [2, 3], [2, 4], [3, 5]] === setLowerBound [2, 1] [[2, 2], [2, 3], [2, 4], [3, 5]]
  [[1, 4], [1, 5], [2, 1], [2, 4]] === setLowerBound [2, 1] [[1, 4], [1, 5], [2, 2], [2, 4]]
  [[1, 4], [2, 4]] === setLowerBound [2, 1] [[1, 4], [2, 1], [2, 3], [2, 4]]
  [[1, 4], [2, 1], [2, 1, 1], [2, 4]] === setLowerBound [2, 1, 1] [[1, 4], [2, 1], [2, 3], [2, 4]]
  where
    range1 = [[2, 2], [2, 3]]
    range2 = [[2, 1], [2, 3]]
    range3 = intersectVersionRanges (orLaterVersion [2, 1]) (orEarlierVersion [2, 3])

test_setUpperBound :: UnitTest
test_setUpperBound = do
  range1 === setUpperBound [2, 3] range1
  range2 === setUpperBound [2, 4] range1
  [[1, 4], [2, 1, 1], [2, 2], [2, 4]] === setUpperBound [2, 1, 1] [[1, 4], [2, 1], [2, 2], [2, 4]]
  where
    range1 = [[2, 2], [2, 3]]
    range2 = [[2, 2], [2, 4]]

test_amendLowerBound :: UnitTest
test_amendLowerBound = do
  range1 === amendLowerBound [1, 5] range1
  [[1, 5], [1, 6]] === amendLowerBound [1, 5] [[0], [1, 6]]
  where
    range1 = [[1, 0], [1, 1], [1, 6], [1, 7]]

domain :: [Version]
domain =
  [[s, m] | s <- [0..2], m <- [0 .. 5]]

takeVersion :: [Version] -> Gen (Maybe (Version, [Version]))
takeVersion pool = do
  count <- Gen.int (Range.constant 0 3)
  pure (uncons (drop count pool))

boundSort :: Gen Bound
boundSort =
  Gen.element [InclusiveBound, ExclusiveBound]

interval :: Version -> Version -> Gen (VersionInterval, Bool)
interval lower upper = do
  lowerSort <- boundSort
  upperSort <- boundSort
  pure (VersionInterval (LowerBound lower lowerSort) (UpperBound upper upperSort), upperSort == InclusiveBound)

intervals :: Gen [VersionInterval]
intervals =
  spin (version0 : domain)
  where
    spin pool = do
      takeVersion pool >>= \case
        Just (lower, rest) ->
          withLower lower rest
        Nothing ->
          pure []

    withLower lower pool =
      takeVersion pool >>= \case
        Just (upper, rest) -> do
          withUpper lower upper rest
        Nothing -> do
          lowerSort <- boundSort
          pure [VersionInterval (LowerBound lower lowerSort) NoUpperBound]

    withUpper lower upper pool = do
      (h, isInclusive) <- interval lower upper
      gap <- Gen.frequency [(5, pure True), (5, pure False)]
      t <- if gap || isInclusive
           then spin pool
           else withLower upper pool
      pure (h : t)

ranges :: Gen VersionRange
ranges = rangeFromIntervals <$> intervals

uppers :: VersionRange -> [Version]
uppers =
  mapMaybe upper . dropEnd 1 . asVersionIntervals
  where
    upper = \case
      VersionInterval _ (UpperBound u _) -> Just u
      _ -> Nothing

hasConnected :: Version -> VersionRange -> (Bool, Bool)
hasConnected bound range =
  foldl step (False, False) (zip ivs (drop 1 ivs))
  where
    ivs = asVersionIntervals range
    step (_, False) (VersionInterval _ (UpperBound u _), VersionInterval (LowerBound l _) _)
      | u == l
      = (True, bound == u)
    step z _ = z

prop_requireLowerBound :: Property
prop_requireLowerBound =
  property do
    range <- forAllWith showP ranges
    let
      boundGens =
        [
          (5, (False,) <$> Gen.element domain),
          (5, (True,) <$> (Gen.element =<< Gen.filter (not . null) (pure (uppers range))))
        ]
    (isUpper, newBound) <- forAllWith (showP . snd) (Gen.frequency boundGens)
    let
      (conn, connIsBound) = hasConnected newBound range
    classify "range has connected intervals" conn
    classify "connected interval at new bound" connIsBound
    classify "bound is one of the uppers" isUpper
    let newRange = requireUpperBound newBound range
    Just newBound === upperBound newRange

test_version :: TestTree
test_version =
  testGroup "version" [
    unitTest "fromList" test_versionList,
    unitTest "bounds" test_rangeBounds,
    unitTest "majors" test_majors,
    unitTest "set lower bound" test_setLowerBound,
    unitTest "set upper bound" test_setUpperBound,
    unitTest "amend lower bound" test_amendLowerBound,
    testProperty "require lower bound" (withTests 1000 prop_requireLowerBound)
  ]
