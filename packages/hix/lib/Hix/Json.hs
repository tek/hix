module Hix.Json where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, fromJSON)
import Exon (exon)

import Hix.Data.Error (Error)
import Hix.Data.Monad (M)
import Hix.Monad (throwM)
import Hix.Optparse (JsonConfig (JsonConfig))

jsonConfig ::
  FromJSON a =>
  (Text -> Error) ->
  JsonConfig ->
  M a
jsonConfig consError (JsonConfig mv) =
  liftIO mv >>= \case
    Left msg -> failure [exon|Invalid JSON: #{toText msg}|]
    Right v -> case fromJSON v of
      Aeson.Success a -> pure a
      Aeson.Error err -> failure [exon|Invalid JSON: #{toText err}
#{show v}|]
  where
    failure = throwM . consError

jsonConfigE ::
  FromJSON a =>
  (Text -> Error) ->
  Either a JsonConfig ->
  M a
jsonConfigE consError =
  either pure (jsonConfig consError)
