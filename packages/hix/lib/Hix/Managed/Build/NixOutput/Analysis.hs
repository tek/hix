module Hix.Managed.Build.NixOutput.Analysis where

import Data.List.Extra (trim)
import Distribution.Compat.CharParsing (anyChar, char, digit, string)
import qualified Distribution.Compat.Parsing as P
import Distribution.Compat.Parsing (manyTill, sepEndByNonEmpty)
import Distribution.Parsec (Parsec (..), simpleParsec)
import Distribution.Simple (Dependency)

import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep)

newtype BuildErrorId =
  BuildErrorId Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

instance Parsec BuildErrorId where
  parsec = do
    manyTill anyChar (string "error")
    manyTill anyChar (char ':')
    manyTill anyChar (string "[GHC-")
    maybe (fail "impossible: invalid digits") (pure . BuildErrorId) . readMaybe =<< P.some digit

data FailureReason =
  Unclear
  |
  BuildError (NonEmpty BuildErrorId)
  |
  BoundsError (NonEmpty Dep)
  deriving stock (Eq, Show)

boundsMarker1 :: Text
boundsMarker1 = "Encountered missing or private dependencies:"

boundsMarker2 :: Text
boundsMarker2 = "Error: Setup: " <> boundsMarker1

boundsMarkers :: [Text]
boundsMarkers = [boundsMarker1, boundsMarker2]

newtype CommaSeparatedDeps =
  CommaSeparatedDeps { deps :: NonEmpty Dependency }
  deriving stock (Eq, Show)

instance Parsec CommaSeparatedDeps where
  parsec =
    CommaSeparatedDeps <$> sepEndByNonEmpty parsec (char ',' *> many (char ' '))

analyzeLog :: [Text] -> FailureReason
analyzeLog log =
  maybe (maybe Unclear BuildError buildErrors) BoundsError (nonEmpty bounds)
  where
    buildErrors = nonEmpty (mapMaybe (simpleParsec . toString) log)

    bounds = concat (catMaybes (takeWhile isJust (parseDeps <$> afterBoundsMarker)))

    afterBoundsMarker = drop 1 (dropWhile (not . flip elem boundsMarkers) log)

    parseDeps line = do
      CommaSeparatedDeps {deps} <- simpleParsec @CommaSeparatedDeps (trim (toString line))
      pure (Dep.fromCabal <$> toList deps)
