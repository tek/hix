{-# language MonadComprehensions #-}

module Hix.Version where

import Data.List.Extra (groupOnKey)
import Distribution.Version (
  Bound (ExclusiveBound),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
  Version,
  VersionInterval (VersionInterval),
  VersionRange,
  alterVersion,
  asVersionIntervals,
  version0,
  versionNumbers,
  )

import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major))

lowerVersion :: VersionRange -> Maybe Version
lowerVersion range =
  [v | VersionInterval (LowerBound v _) _ <- head (asVersionIntervals range), v /= version0]

exclusiveUpperVersion :: VersionRange -> Maybe Version
exclusiveUpperVersion range =
  [v | VersionInterval _ (UpperBound v ExclusiveBound) <- last (asVersionIntervals range)]

upperVersion :: VersionRange -> Maybe Version
upperVersion range =
  [v | VersionInterval _ (UpperBound v _) <- last (asVersionIntervals range)]

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

isMajor :: Int -> Int -> Major -> Bool
isMajor s m Major {prefix} =
  prefix == [s, m]

majorOlder :: Int -> Int -> Major -> Bool
majorOlder s m Major {prefix} =
  prefix < [s, m]

majorNewer :: Int -> Int -> Major -> Bool
majorNewer s m Major {prefix} =
  prefix > [s, m]

allMajors :: [Version] -> [Major]
allMajors =
  mapMaybe (uncurry cons) . groupOnKey currentMajor
  where
    cons prefix = \case
      (h : t) -> Just (Major {prefix, versions = h :| t})
      [] -> Nothing

majorsBefore :: Int -> Int -> [Version] -> [Major]
majorsBefore s m =
  takeWhile (majorOlder s m) . allMajors

majorsFrom :: Int -> Int -> [Version] -> [Major]
majorsFrom s m =
  dropWhile (majorOlder s m) . allMajors

onlyMajor :: Int -> Int -> [Version] -> Maybe Major
onlyMajor s m =
  find (isMajor s m) . allMajors

versionsFrom :: Version -> [Version] -> [Major]
versionsFrom start =
  allMajors . dropWhile (< start)

versionsBetween :: Version -> Version -> [Version] -> [Major]
versionsBetween l u =
  allMajors . takeWhile (< u) . dropWhile (< l)
