{-# language MonadComprehensions #-}

module Hix.Version where

import Data.List.NonEmpty.Extra (foldl1')
import Distribution.Parsec (eitherParsec)
import Distribution.Version (
  Bound (ExclusiveBound, InclusiveBound),
  LowerBound (LowerBound),
  UpperBound (NoUpperBound, UpperBound),
  Version,
  VersionInterval (VersionInterval),
  VersionRange,
  VersionRangeF (..),
  alterVersion,
  asVersionIntervals,
  earlierVersion,
  intersectVersionRanges,
  laterVersion,
  orEarlierVersion,
  orLaterVersion,
  simplifyVersionRange,
  unionVersionRanges,
  version0,
  versionNumbers,
  )
import Exon (exon)

import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major))

unsafeRange :: Text -> VersionRange
unsafeRange spec =
  either crash id (eitherParsec (toString spec))
  where
    crash err = error ([exon|Version range parse error for #{toString spec}: #{err}|])

rangeFromInterval :: VersionInterval -> VersionRange
rangeFromInterval (VersionInterval (LowerBound lowerV lowerB) upper) =
  simplifyVersionRange (addUpper upper (consLower lowerB lowerV))
  where
    consLower = \case
      ExclusiveBound -> laterVersion
      InclusiveBound -> orLaterVersion

    addUpper = \case
      NoUpperBound -> id
      UpperBound upperV upperB -> intersectVersionRanges (consUpper upperB upperV)

    consUpper = \case
      ExclusiveBound -> earlierVersion
      InclusiveBound -> orEarlierVersion

rangeFromIntervals :: [VersionInterval] -> VersionRange
rangeFromIntervals = \case
  [] -> laterVersion version0
  h : t -> foldl1' unionVersionRanges (rangeFromInterval <$> (h :| t))

-- TODO check what to do with exclusivity
lowerBound :: VersionRange -> Maybe Version
lowerBound range =
  [v | VersionInterval (LowerBound v _) _ <- head (asVersionIntervals range), v /= version0]

lowerBoundRange :: VersionRange -> VersionRange
lowerBoundRange range =
  orLaterVersion (fromMaybe version0 (lowerBound range))

upperBound :: VersionRange -> Maybe Version
upperBound range =
  [v | VersionInterval _ (UpperBound v ExclusiveBound) <- last (asVersionIntervals range)]

upperVersion :: VersionRange -> Maybe Version
upperVersion range =
  [v | VersionInterval _ (UpperBound v _) <- last (asVersionIntervals range)]

withIntervals :: ([VersionInterval] -> [VersionInterval]) -> VersionRange -> VersionRange
withIntervals f =
  simplifyVersionRange .
  rangeFromIntervals .
  f .
  asVersionIntervals

-- TODO This probably doesn't do the right thing when there is an interval that's entirely below the new bound.
-- What happens is that a new interval will be created, so that setting 1.6 as new bound for @>=1.1 && <1.3@ will result
-- in @>=1.1 && <1.3 || >= 1.6@.
--
-- But we call this after having decided that the project will _not_ build for any version lower than 1.6.
-- However, if the given bounds were preexisting, we wouldn't even select 1.6 as a candidate, so it doesn't matter too
-- much, and the project would have failed to build anyway before running @lower.init@.
--
-- On the other hand, if we want to use it for @latest@ to adjust the bounds, it creates some issues.
setLowerBound :: Version -> VersionRange -> VersionRange
setLowerBound bound =
  withIntervals spin
  where
    spin = \case
      [] -> [VersionInterval (LowerBound bound InclusiveBound) NoUpperBound]

      interval : rest
        | VersionInterval _ (UpperBound upper ExclusiveBound) <- interval
        , upper <= bound
        -> interval : spin rest

        | VersionInterval _ (UpperBound upper InclusiveBound) <- interval
        , upper < bound
        -> interval : spin rest

        | VersionInterval _ upper <- interval
        -> VersionInterval (LowerBound bound InclusiveBound) upper : rest

setUpperBound :: Version -> VersionRange -> VersionRange
setUpperBound bound =
  withIntervals (reverse . spin . reverse)
  where
    spin = \case
      [] -> [VersionInterval (LowerBound version0 InclusiveBound) (UpperBound bound ExclusiveBound)]

      interval : rest
        | VersionInterval (LowerBound lower ExclusiveBound) _ <- interval
        , lower >= bound
        -> interval : spin rest

        | VersionInterval (LowerBound lower InclusiveBound) _ <- interval
        , lower > bound
        -> interval : spin rest

        | VersionInterval lower _ <- interval
        -> VersionInterval lower (UpperBound bound ExclusiveBound) : rest

requireUpperBound :: Version -> VersionRange -> VersionRange
requireUpperBound bound =
  withIntervals (spin version0)
  where
    spin highest = \case
      [] -> [VersionInterval (LowerBound highest InclusiveBound) (UpperBound bound ExclusiveBound)]

      [VersionInterval lower _] -> [VersionInterval (clampLower lower) (UpperBound bound ExclusiveBound)]

      interval : rest
        | VersionInterval _ (UpperBound upper _) <- interval
        , upper < bound
        -> interval : spin upper rest

        | VersionInterval lower _ <- interval
        -> [VersionInterval (clampLower lower) (UpperBound bound ExclusiveBound)]

    clampLower (LowerBound lower boundSort)
        | lower >= bound = LowerBound (prevMajor bound) InclusiveBound
        | otherwise = LowerBound lower boundSort

amendLowerBound :: Version -> VersionRange -> VersionRange
amendLowerBound bound =
  withIntervals \case
    [VersionInterval (LowerBound lower _) upper] | lower == version0 ->
      [VersionInterval (LowerBound bound InclusiveBound) upper]
    a -> a

majorParts :: Version -> Maybe (Int, Int)
majorParts =
  versionNumbers >>> \case
    [] -> Nothing
    [0] -> Nothing
    [s] -> Just (s, 0)
    s : m : _ -> Just (s, m)

majorPrefix :: Version -> Maybe [Int]
majorPrefix =
  majorParts >>> fmap \case
    (s, m) -> [s, m]

hasMajor :: Int -> Int -> Version -> Bool
hasMajor s m candidate =
  case take 2 (versionNumbers candidate) of
    [s'] -> s' == s && m == 0
    s' : m' : _ -> s' == s && m' == m
    _ -> False

minMajor :: Int -> Int -> Version -> Bool
minMajor s m candidate =
  case take 2 (versionNumbers candidate) of
    [s'] -> s' > s || (s' == s && m == 0)
    s' : m' : _ -> s' > s || (s' == s && m' >= m)
    _ -> False

beforeMajor :: Int -> Int -> Version -> Bool
beforeMajor s m candidate =
  case take 2 (versionNumbers candidate) of
    [s'] -> s' < s || (s' == s && m > 0)
    s' : m' : _ -> s' < s || (s' == s && m' < m)
    _ -> False

lastMajor :: [Version] -> [Version]
lastMajor =
  reverse . fst . foldl step ([], (0, 0))
  where
    step (z, cur) a
      | Just pre <- majorParts a
      , pre /= cur
      = ([a], pre)
      | otherwise
      = (a : z, cur)

lastMajorBefore :: Int -> Int -> [Version] -> [Version]
lastMajorBefore s m =
  reverse . fst . foldl step ([], (0, 0))
  where
    step (z, cur) a
      | Just (s', m') <- mp
      , s' > s || (s' == s && m' >= m)
      = (z, cur)
      | Just pre <- mp
      , pre /= cur
      = ([a], pre)
      | otherwise
      = (a : z, cur)
      where
        mp = majorParts a

majorsBefore :: Int -> Int -> [Version] -> [Major]
majorsBefore s m =
  reverse . add . foldl step ([], [], (0, 0))
  where
    step (z, maj, cur) a
      | Just (s', m') <- mp
      , s' > s || (s' == s && m' >= m)
      = (z, maj, cur)
      | Just pre <- mp
      , pre /= cur
      = (add (z, maj, cur), [a], pre)
      | otherwise
      = (z, a : maj, cur)
      where
        mp = majorParts a

    add (z, maj, (s', m')) =
      case reverse maj of
        [] -> z
        h : t -> cons s' m' h t : z

    cons s' m' h t = Major {prefix = [s', m'], versions = h :| t}

secondMajorBefore :: Int -> Int -> [Version] -> [Version]
secondMajorBefore s m allVersions =
  case take 2 (reverse (majorsBefore s m allVersions)) of
    [Major {versions}] -> toList versions
    [_, Major {versions}] -> toList versions
    _ -> []

lowerRangeF :: VersionRangeF VersionRange -> VersionRange
lowerRangeF = \case
  ThisVersionF v -> orLaterVersion v
  OrLaterVersionF v -> orLaterVersion v
  LaterVersionF v -> laterVersion v
  IntersectVersionRangesF l _ -> l
  _ -> orLaterVersion version0

nextMajor :: Version -> Version
nextMajor =
  alterVersion \case
    [] -> [0, 1]
    [s] -> [s, 1]
    s : m : _ -> [s, m + 1]

prevMajor :: Version -> Version
prevMajor =
  alterVersion \case
    [s] | s > 0 -> [s - 1, 99]
    s : 0 : _ | s > 0 -> [s - 1, 99]
    s : m : _ | s > 0, m > 0 -> [s, m - 1]
    _ -> [0]

currentMajor :: Version -> Version
currentMajor = alterVersion (take 2)
