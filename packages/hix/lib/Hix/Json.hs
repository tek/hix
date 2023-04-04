module Hix.Json where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, fromJSON)
import Exon (exon)

import Hix.Data.Error (Error (GhciError))
import Hix.Monad (M)
import Hix.Optparse (JsonConfig (JsonConfig))

jsonConfig ::
  FromJSON a =>
  JsonConfig ->
  M a
jsonConfig (JsonConfig mv) =
  liftIO mv >>= \case
    Left msg -> failure [exon|Invalid JSON: #{toText msg}|]
    Right v -> case fromJSON v of
      Aeson.Success a -> pure a
      Aeson.Error err -> failure [exon|Invalid JSON: #{toText err}
#{show v}|]
  where
    failure = lift . throwE . GhciError
