module Hix.Json where

import Data.Aeson (FromJSON, eitherDecodeFileStrict', eitherDecodeStrict')
import Exon (exon)
import Path (File, toFilePath)

import Hix.Data.Error (Error (..))
import Hix.Data.Json (JsonContext (..))
import Hix.Data.Monad (M)
import Hix.Data.PathSpec (PathSpec (..))
import Hix.Maybe (fromMaybeA)
import Hix.Monad (appContext, catchM, clientError, eitherClient, tryIOM)
import Hix.Path (resolvePathSpec)
import Hix.Pretty (showP)

decodeJsonInplace ::
  FromJSON a =>
  PathSpec File ->
  Maybe a
decodeJsonInplace = \case
  PathUser spec -> rightToMaybe (eitherDecodeStrict' (encodeUtf8 spec))
  PathConcrete _ -> Nothing

decodeJsonFile ::
  FromJSON a =>
  PathSpec File ->
  M a
decodeJsonFile spec = do
  path <- resolvePathSpec spec
  result <- tryIOM (eitherDecodeFileStrict' (toFilePath path))
  eitherClient (first toText result)

jsonContext ::
  FromJSON a =>
  JsonContext ->
  M a
jsonContext (JsonContext spec) =
  appContext "decoding the Hix context CLI argument" do
    catchM (fromMaybeA (decodeJsonFile spec) (decodeJsonInplace spec)) \ err ->
      clientError [exon|Invalid JSON or path: #{showP err.message}|]

resolveContext :: FromJSON a => Either a JsonContext -> M a
resolveContext = either pure jsonContext
