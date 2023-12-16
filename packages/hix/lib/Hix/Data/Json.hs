module Hix.Data.Json where

import Data.Aeson (FromJSON (parseJSON), Key, Object, (.:?))
import Data.Aeson.Types (Parser)
import Distribution.Parsec (Parsec, eitherParsec)

aesonParsec ::
  Parsec a =>
  String ->
  Parser a
aesonParsec =
  leftA fail . eitherParsec

newtype JsonParsec a =
  JsonParsec a
  deriving stock (Eq, Show, Generic)

instance Parsec a => FromJSON (JsonParsec a) where
  parseJSON v = do
    raw <- parseJSON v
    JsonParsec <$> aesonParsec raw

jsonParsec :: JsonParsec a -> a
jsonParsec = coerce

foldMissing ::
  Monoid a =>
  FromJSON a =>
  Object ->
  Key ->
  Parser a
foldMissing o k =
  fold <$> o .:? k
