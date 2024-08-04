module Hix.Data.VersionBounds where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object, String), object, (.:?), (.=))
import Data.Aeson.Types (typeMismatch)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (
  Version,
  VersionRange,
  earlierVersion,
  intersectVersionRanges,
  orEarlierVersion,
  orLaterVersion,
  simplifyVersionRange,
  thisVersion,
  )

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Data.Json (aesonParsec, jsonParsec)
import Hix.Data.Version (range0)
import Hix.Pretty (showP)
import Hix.Version (lowerVersion, upperVersion, nextMajor, prevMajor)

data Bound =
  BoundLower
  |
  BoundUpper
  deriving stock (Eq, Show, Generic)

instance Pretty Bound where
  pretty = \case
    BoundLower -> "lower"
    BoundUpper -> "upper"

data VersionBounds =
  VersionBounds {
    lower :: Maybe Version,
    upper :: Maybe Version
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (EncodeNix)

instance Semigroup VersionBounds where
  left <> right = VersionBounds {lower = left.lower <|> right.lower, upper = left.upper <|> right.upper}

instance Monoid VersionBounds where
  mempty = VersionBounds {lower = Nothing, upper = Nothing}

unsafeVersionBoundsFromRange :: VersionRange -> VersionBounds
unsafeVersionBoundsFromRange range =
  VersionBounds {lower = lowerVersion range, upper = upperVersion range}

instance FromJSON VersionBounds where
  parseJSON = \case
    String s -> do
      range <- aesonParsec (toString s)
      pure (unsafeVersionBoundsFromRange range)
    Object o -> do
      lower <- fmap jsonParsec <$> o .:? "lower"
      upper <- fmap jsonParsec <$> o .:? "upper"
      pure VersionBounds {..}
    v -> do
      let
        expected =
          "a managed bound as either a string containing a Cabal-style range or an object with 'lower' and/or 'upper'"
          <>
          " as keys and simple versions as values"
      typeMismatch expected v

instance ToJSON VersionBounds where
  toJSON VersionBounds {..} =
    object [
      "lower" .= toJSON (showP @Text <$> lower),
      "upper" .= toJSON (showP @Text <$> upper)
    ]

maybeRange ::
  (Bound -> Version -> VersionRange) ->
  VersionBounds ->
  Maybe VersionRange
maybeRange mkRange bounds
  | Just l <- bounds.lower
  , Just u <- bounds.upper
  , u <= l
  = Just (thisVersion l)

  | Just l <- lower
  , Just u <- upper
  = Just (simplifyVersionRange (intersectVersionRanges l u))

  | Just l <- lower
  , Nothing <- upper
  = Just l

  | Nothing <- lower
  , Just u <- upper
  = Just u

  | Nothing <- lower
  , Nothing <- upper
  = Nothing
  where
    lower = mkRange BoundLower <$> bounds.lower
    upper = mkRange BoundUpper <$> bounds.upper

-- | Return a @==lower@ range for invalid bounds.
-- The constructors prevent this from happening, so it's maybe for tests?
-- Probably better to make the type abstract and crash.
maybeMajorRange ::
  VersionBounds ->
  Maybe VersionRange
maybeMajorRange =
  maybeRange \case
    BoundLower -> orLaterVersion
    BoundUpper -> earlierVersion

majorRange :: VersionBounds -> VersionRange
majorRange = fromMaybe range0 . maybeMajorRange

instance Pretty VersionBounds where
  pretty = pretty . majorRange

maybeInclusiveRange :: VersionBounds -> Maybe VersionRange
maybeInclusiveRange =
  maybeRange \case
    BoundLower -> orLaterVersion
    BoundUpper -> orEarlierVersion

inclusiveRange :: VersionBounds -> VersionRange
inclusiveRange = fromMaybe range0 . maybeInclusiveRange

anyBounds :: VersionBounds
anyBounds =
  VersionBounds {lower = Nothing, upper = Nothing}

versionBounds :: Version -> Version -> VersionBounds
versionBounds lower upper =
  VersionBounds {lower = Just lower, upper = Just upper}

fromLower :: Version -> VersionBounds
fromLower lower =
  VersionBounds {lower = Just lower, upper = Nothing}

fromUpper :: Version -> VersionBounds
fromUpper upper =
  VersionBounds {lower = Nothing, upper = Just upper}

exactVersion :: Version -> VersionBounds
exactVersion version = versionBounds version version

withLower :: Version -> VersionBounds -> VersionBounds
withLower lower VersionBounds {upper} =
  VersionBounds {lower = Just lower, upper = clamp =<< upper}
  where
    clamp old | old < lower = Nothing
              | otherwise = Just old

withUpper :: Version -> VersionBounds -> VersionBounds
withUpper upper VersionBounds {lower} =
  VersionBounds {lower = clamp =<< lower, upper = Just upper}
  where
    clamp old | old > upper = Nothing
              | otherwise = Just old

amendUpper :: Version -> VersionBounds -> VersionBounds
amendUpper new bounds
  | Just _ <- bounds.upper
  = bounds
  | otherwise
  = withUpper new bounds

updateWithCorrection :: VersionBounds -> VersionBounds -> VersionBounds
updateWithCorrection VersionBounds {lower = Just lower, upper = Nothing} VersionBounds {upper = Just upper} =
  VersionBounds {lower = Just lower, upper = Just if upper > lower then upper else nextMajor lower}
updateWithCorrection VersionBounds {lower = Nothing, upper = Just upper} VersionBounds {lower = Just lower} =
  VersionBounds {lower = Just if lower < upper then lower else prevMajor upper, upper = Just upper}
updateWithCorrection new old = new <> old
