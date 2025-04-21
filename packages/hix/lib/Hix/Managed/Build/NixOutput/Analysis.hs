module Hix.Managed.Build.NixOutput.Analysis where

import Control.Lens ((.~))
import Control.Lens.Regex.Text (match, regex)
import Data.List.Extra (firstJust, trim)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Distribution.Compat.CharParsing (CharParsing, anyChar, char, digit, string)
import qualified Distribution.Compat.Parsing as P
import Distribution.Compat.Parsing (manyTill, sepEndByNonEmpty, skipMany, try)
import Distribution.Parsec (CabalParsing, Parsec (..), simpleParsec)
import Distribution.Pretty (Pretty (..))
import Distribution.Simple (Dependency)
import Exon (exon)

import qualified Hix.Data.Dep as Dep
import Hix.Data.Dep (Dep)
import Hix.Data.PackageName (PackageName)
import Hix.Managed.Report (pluralLength)
import Hix.Pretty (prettyText, showP)

sanitizeOutput :: Text -> Text
sanitizeOutput =
  [regex|(\x{9b}|\x{1b}\[)[0-?]*[ -\/]*[@-~]|] . match .~ ""

newtype BuildErrorId =
  BuildErrorId Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

instance Parsec BuildErrorId where
  parsec = do
    manyTill anyChar (try (string "error"))
    manyTill anyChar (char ':')
    manyTill anyChar (try (string "[GHC-"))
    maybe (fail "impossible: invalid digits") (pure . BuildErrorId) . readMaybe =<< (P.some digit <* skipMany anyChar)

newtype UnknownDepMessage =
  UnknownDepMessage PackageName
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

tillQuote ::
  ∀ a m .
  CharParsing m =>
  IsString a =>
  m a
tillQuote =
  fromString <$> manyTill anyChar (char '\'')

unknownDepHix :: CabalParsing m => m UnknownDepMessage
unknownDepHix = do
  _ <- manyTill anyChar (try (string "The Cabal config for '"))
  _local <- tillQuote @String
  string " in the env '"
  _env <- tillQuote @String
  string " has a dependency on the nonexistent package '"
  tillQuote <* skipMany anyChar

unknownDepNixpkgs :: CabalParsing m => m UnknownDepMessage
unknownDepNixpkgs = do
  _ <- manyTill anyChar (try (string "function 'anonymous lambda' called without required argument '"))
  tillQuote <* skipMany anyChar

-- TODO maybe we can add a module option that toggles errors in json or something, with which we can reliably extract
-- this information
instance Parsec UnknownDepMessage where
  parsec = try unknownDepHix <|> unknownDepNixpkgs

data FailureReason =
  Unclear
  |
  BuildError (NonEmpty BuildErrorId)
  |
  BoundsError (NonEmpty Dep)
  |
  UnknownDep
  deriving stock (Eq, Show)

listThree :: NonEmpty Text -> Text
listThree things =
  [exon|#{examples}#{ellipsis}|]
  where
    (three, rest) = NonEmpty.splitAt 3 things
    examples = Text.intercalate ", " three
    ellipsis = if null rest then "" else "..."

instance Pretty FailureReason where
  pretty = \case
    Unclear -> "unclear"
    BuildError ids ->
      prettyText [exon|build error#{pluralLength ids} #{listThree (coerce ids)}|]
    BoundsError deps ->
      prettyText [exon|bounds error#{pluralLength deps} #{listThree (showP . (.package) <$> deps)}|]
    UnknownDep ->
      "unknown package"

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
    CommaSeparatedDeps <$> sepEndByNonEmpty parsec (char ',' *> many (char ' ')) <* skipMany anyChar

analyzeLog :: [Text] -> FailureReason
analyzeLog log =
  fromMaybe Unclear variants
  where
    variants =
      (BoundsError <$> bounds)
      <|>
      (BuildError <$> buildErrors)

    buildErrors = nonEmpty (mapMaybe simpleParsec logStrings)

    bounds = nonEmpty (concat (catMaybes (takeWhile isJust (parseDeps <$> afterBoundsMarker))))

    afterBoundsMarker = drop 1 (dropWhile (not . flip elem boundsMarkers) log)

    parseDeps line = do
      CommaSeparatedDeps {deps} <- simpleParsec @CommaSeparatedDeps (trim (toString line))
      pure (Dep.fromCabal <$> toList deps)

    logStrings = toString <$> log

analyzeEarlyFailure :: NonEmpty Text -> Maybe (PackageName, FailureReason)
analyzeEarlyFailure log =
  firstJust simpleParsec (toList logStrings) <&> \ (UnknownDepMessage name) -> (name, UnknownDep)
  where
    logStrings = toString . sanitizeOutput <$> log
