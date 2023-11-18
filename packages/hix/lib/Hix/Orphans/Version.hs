{-# options_ghc -Wno-orphans #-}

module Hix.Orphans.Version where

import Distribution.Version (
  LowerBound (LowerBound),
  UpperBound (NoUpperBound, UpperBound),
  Version,
  VersionInterval (VersionInterval),
  VersionRange,
  asVersionIntervals,
  earlierVersion,
  intersectVersionRanges,
  mkVersion,
  orLaterVersion,
  unionVersionRanges,
  version0,
  versionNumbers,
  )
import GHC.Exts (IsList (..))

instance IsList Version where
  type Item Version = Int

  fromList = mkVersion

  toList = versionNumbers

instance IsList VersionRange where
  type Item VersionRange = Version

  fromList = \case
    [] -> orLaterVersion version0
    [v] -> orLaterVersion v
    [l, u] -> range l u
    l : u : rest -> unionVersionRanges (range l u) (fromList rest)
    where
      range l u = intersectVersionRanges (orLaterVersion l) (earlierVersion u)

  toList range = do
    VersionInterval (LowerBound v _) upper <- asVersionIntervals range
    v : fromUpper upper
    where
      fromUpper = \case
        NoUpperBound -> []
        UpperBound v _ -> [v]
