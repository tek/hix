module Hix.Hackage where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))

import Hix.Data.Monad (M)
import Hix.Managed.Handlers.HackageClient (
  HackageClient (..),
  HackageError (..),
  HackageRequest (..),
  HackageResponse (HackageResponseJson),
  )
import Hix.Monad (fatalError)

data HackageVersions =
  HackageVersions {
    versions :: [String]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON HackageVersions where
  parseJSON = withObject "HackageVersions" \ o -> HackageVersions <$> o .: "normal-version"

hackageRequest ::
  ∀ a b .
  (a -> Either Text b) ->
  HackageClient ->
  HackageRequest a ->
  M (Either HackageError b)
hackageRequest process HackageClient {request = run} request = do
  response <- run request
  pure (first HackageParseError . process =<< response)

hackageGet ::
  ∀ a b .
  (a -> Either Text b) ->
  HackageClient ->
  Text ->
  HackageResponse a ->
  M (Either HackageError b)
hackageGet process client path accept =
  hackageRequest process client HackageRequest {
    method = "GET",
    path,
    body = Nothing,
    query = Nothing,
    accept
  }

hackagePostJson ::
  ∀ a b body .
  FromJSON a =>
  Typeable a =>
  ToJSON body =>
  (a -> Either Text b) ->
  HackageClient ->
  Text ->
  body ->
  M (Either HackageError b)
hackagePostJson process client path body =
  hackageRequest process client HackageRequest {
    method = "POST",
    path,
    body = Just (Right (Aeson.encode body)),
    query = Nothing,
    accept = HackageResponseJson
  }

hackagePostQuery ::
  ∀ a b .
  (a -> Either Text b) ->
  HackageClient ->
  Text ->
  NonEmpty (Text, Text) ->
  HackageResponse a ->
  M (Either HackageError b)
hackagePostQuery process client path query accept =
  hackageRequest process client HackageRequest {
    method = "POST",
    path,
    body = Nothing,
    query = Just (bimap encodeUtf8 encodeUtf8 <$> query),
    accept
  }

hackagePostForm ::
  ∀ a b .
  (a -> Either Text b) ->
  HackageClient ->
  Text ->
  NonEmpty (Text, Text) ->
  HackageResponse a ->
  M (Either HackageError b)
hackagePostForm process client path fields accept =
  hackageRequest process client HackageRequest {
    method = "POST",
    path,
    body = Just (Left fields),
    query = Nothing,
    accept
  }

hackagePut ::
  ∀ a b .
  FromJSON a =>
  Typeable a =>
  (a -> Either Text b) ->
  HackageClient ->
  Text ->
  M (Either HackageError b)
hackagePut process client path =
  hackageRequest process client HackageRequest {
    method = "PUT",
    path,
    body = Nothing,
    query = Nothing,
    accept = HackageResponseJson
  }

fatalHackageRequest :: Either HackageError a -> M a
fatalHackageRequest =
  leftA $ fatalError . \case
    HackageNotFound -> "Not found"
    HackageFatal msg -> msg
    HackageParseError msg -> msg
