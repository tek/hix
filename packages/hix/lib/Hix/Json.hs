module Hix.Json where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, fromJSON)
import Exon (exon)

import Hix.Data.Json (JsonConfig (..))
import Hix.Data.Monad (M)
import Hix.Monad (fatalError)

jsonConfig ::
  FromJSON a =>
  JsonConfig ->
  M a
jsonConfig (JsonConfig mv) =
  liftIO mv >>= \case
    Left msg -> fatalError [exon|Invalid JSON: #{toText msg}|]
    Right v -> case fromJSON v of
      Aeson.Success a -> pure a
      Aeson.Error err -> fatalError [exon|Invalid JSON: #{toText err}
#{show v}|]

jsonConfigE :: FromJSON a => Either a JsonConfig -> M a
jsonConfigE = either pure jsonConfig
