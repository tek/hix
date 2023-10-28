module Hix.Version where

import Distribution.Version (
  Version,
  VersionRange,
  VersionRangeF (..),
  alterVersion,
  earlierVersion,
  embedVersionRange,
  fromVersionIntervals,
  intersectVersionRanges,
  laterVersion,
  orLaterVersion,
  projectVersionRange,
  simplifyVersionRange,
  toVersionIntervals,
  unionVersionRanges,
  version0,
  withinRange,
  )

lowerRange :: VersionRangeF VersionRange -> VersionRange
lowerRange = \case
  ThisVersionF v -> orLaterVersion v
  OrLaterVersionF v -> orLaterVersion v
  LaterVersionF v -> laterVersion v
  IntersectVersionRangesF l _ -> l
  _ -> orLaterVersion version0

checkVersion :: VersionRange -> Version -> Maybe VersionRange
checkVersion version latest =
  cons <$> change
  where
    cons new = simplifyVersionRange new

    change = extendHighest (fromVersionIntervals (toVersionIntervals version))

    extendHighest =
      projectVersionRange >>> \case
        UnionVersionRangesF l r
          | withinRange latest l -> Nothing
          | otherwise -> unionVersionRanges l <$> extendHighest r
        r@(IntersectVersionRangesF _ _) | withinRange latest (embedVersionRange r) -> Nothing
        v -> Just (intersectVersionRanges (clampLower (lowerRange v)) bumpMajorRange)

    clampLower =
      projectVersionRange >>> \case
        OrLaterVersionF v -> orLaterVersion (clamp v)
        LaterVersionF v -> laterVersion (clamp v)
        v -> embedVersionRange v

    clamp v | v == version0 = majorLower
            | otherwise = v

    majorLower = alterVersion (take 2) latest

    bumpMajorRange = earlierVersion (alterVersion bumpMajor latest)

    bumpMajor = \case
      [] -> [0, 1]
      [s] -> [s, 1]
      s : m : _ -> [s, m + 1]
