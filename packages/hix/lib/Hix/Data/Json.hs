module Hix.Data.Json where

import Data.Aeson (FromJSON (parseJSON), Key, Object, (.:?))
import Data.Aeson.Types (Parser)
import Distribution.Parsec (Parsec, eitherParsec)
import Path (File)

import Hix.Data.PathSpec (PathSpec)

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

useMissing ::
  FromJSON a =>
  a ->
  Object ->
  Key ->
  Parser a
useMissing a o k =
  fromMaybe a <$> o .:? k

defMissing ::
  Default a =>
  FromJSON a =>
  Object ->
  Key ->
  Parser a
defMissing =
  useMissing def

newtype JsonEither a b =
  JsonEither (Either a b)
  deriving stock (Eq, Show)

jsonEither :: JsonEither a b -> Either a b
jsonEither = coerce

instance (FromJSON a, FromJSON b) => FromJSON (JsonEither a b) where
  parseJSON v =
    JsonEither <$> ((Right <$> parseJSON v) <|> (Left <$> parseJSON v))

newtype JsonContext =
  JsonContext { unJsonConfig :: PathSpec File }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)
