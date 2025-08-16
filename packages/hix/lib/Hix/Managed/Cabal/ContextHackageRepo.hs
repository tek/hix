module Hix.Managed.Cabal.ContextHackageRepo where

import Control.Lens (Lens', (.~))
import Data.Char (toLower)
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import Distribution.Parsec (simpleParsec)
import Exon (exon)
import Prelude hiding (bool)

import Hix.CabalParsec (unsafeParsec)
import qualified Hix.Color as Color
import Hix.Managed.Cabal.Data.ContextHackageRepo (
  ContextHackageLocation (..),
  ContextHackagePassword (..),
  ContextHackageRepo (..),
  ContextHackageSecret (..),
  ContextHackageToken (..),
  contextHackageRepo,
  )
import Hix.Managed.Cabal.Data.HackageLocation (HackageSecret (..), HackageUser (..))
import Hix.Managed.Cabal.Data.HackageRepo (HackageName, centralName)

update' ::
  Bool ->
  Lens' ContextHackageRepo (Maybe a) ->
  (String -> Maybe a) ->
  String ->
  String ->
  Either String (ContextHackageRepo -> ContextHackageRepo)
update' redact lens parse field spec = do
  value <- maybeToRight [exon|Invalid value for Hackage field #{toString (Color.blue field)}: #{display}|] (parse spec)
  pure (lens .~ Just value)
  where
    display | redact = "<redacted>"
            | otherwise = spec

update ::
  Lens' ContextHackageRepo (Maybe a) ->
  (String -> Maybe a) ->
  String ->
  String ->
  Either String (ContextHackageRepo -> ContextHackageRepo)
update = update' False

text :: (Text -> a) -> String -> Maybe a
text cons = Just . cons . toText

bool :: String -> Maybe Bool
bool value =
  case toLower <$> value of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing

fields :: Map String (String -> String -> Either String (ContextHackageRepo -> ContextHackageRepo))
fields =
  [
    ("enable", update #enable bool),
    ("location", update #location (text ContextHackageLocation)),
    ("user", update #user (text HackageUser)),
    ("password", update' True #password (text (ContextHackagePassword . SecretPlain . HackageSecret))),
    ("token", update' True #token (text (ContextHackageToken . SecretPlain . HackageSecret))),
    ("secure", update #secure bool),
    ("keys", update #keys (nonEmpty . Text.splitOn "," . toText)),
    ("indexState", update #indexState simpleParsec),
    ("solver", update #solver bool),
    ("publish", update #publish bool)
  ]

fieldUpdater :: String -> String -> Either String (ContextHackageRepo -> ContextHackageRepo)
fieldUpdater field value = do
  f <- maybeToRight [exon|Invalid Hackage repo field: #{field}|] (fields !? field)
  f field value

unsafeCentralHackageContextFixed :: (HackageName, ContextHackageRepo)
unsafeCentralHackageContextFixed =
  (centralName, (contextHackageRepo centralName) {indexState = Just (unsafeParsec ("2025-01-01T00:00:00Z" :: String))})
