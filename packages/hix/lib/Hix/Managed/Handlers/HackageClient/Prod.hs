{-# language MonadComprehensions #-}

module Hix.Managed.Handlers.HackageClient.Prod where

import Data.Aeson (Value, eitherDecodeStrict')
import Exon (exon)
import Network.HTTP.Client (
  Manager,
  Request (..),
  RequestBody (RequestBodyLBS),
  Response (..),
  applyBasicAuth,
  defaultRequest,
  httpLbs,
  setQueryString,
  )
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS)
import Network.HTTP.Types (
  Status (statusCode, statusMessage),
  hAccept,
  hContentType,
  statusIsClientError,
  statusIsServerError,
  statusIsSuccessful,
  )

import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Hackage (hackagePostQuery, hackagePut)
import Hix.Http (httpManager)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose, hackagesFor)
import Hix.Managed.Cabal.Data.HackageLocation (
  HackageHost (..),
  HackageLocation (..),
  HackagePassword (HackagePassword),
  HackageTls (..),
  HackageUser (..),
  hackageTlsBool,
  )
import qualified Hix.Managed.Cabal.Data.HackageRepo as HackageRepo
import Hix.Managed.Cabal.Data.HackageRepo (HackageDescription, HackageRepo (HackageRepo))
import Hix.Managed.Cabal.HackageLocation (hackagePort)
import Hix.Managed.Handlers.HackageClient (
  HackageClient (..),
  HackageError (..),
  HackageRequest (..),
  HackageResponse (..),
  )
import Hix.Maybe (fromMaybeA)
import Hix.Monad (appContextDebug)
import Hix.Network (Port)

data HackageResources =
  HackageResources {
    manager :: Manager,
    description :: HackageDescription,
    location :: HackageLocation
  }

parseResult ::
  LByteString ->
  HackageResponse a ->
  Either HackageError a
parseResult body = \case
  HackageResponseJson ->
    first wrapError (eitherDecodeStrict' (toStrict body))
  HackageResponseHtml ->
    Right (decodeUtf8 body)
  HackageNoResponse ->
    unit
  where
    wrapError err = HackageFatal [exon|Hackage response parse error: #{toText err}|]

baseRequest :: HackageLocation -> HackageRequest a -> Request
baseRequest location@HackageLocation {host = HackageHost host, tls} HackageRequest {..} =
  defaultRequest {
    host = encodeUtf8 host,
    port = fromIntegral (hackagePort location),
    secure = hackageTlsBool tls,
    method = encodeUtf8 method,
    path = encodeUtf8 path,
    requestHeaders = acceptHeader : maybeToList contentHeader
  }
  where
    acceptHeader
      | HackageResponseJson <- accept = (hAccept, "application/json")
      | otherwise = (hAccept, "text/html")

    contentHeader
      | Just (Right _) <- body = Just (hContentType, "application/json")
      | Just _ <- query = Just (hContentType, "application/x-www-form-urlencoded")
      | otherwise = Nothing

nativeRequest :: HackageLocation -> HackageRequest a -> M Request
nativeRequest location request@HackageRequest {..} = do
  addBody body (addAuth location.auth (addQuery query (baseRequest location request)))
  where
    addBody = maybe pure \case
      Right bs -> \ r -> pure r { requestBody = RequestBodyLBS bs }
      Left fields -> formDataBody [partBS key (encodeUtf8 value) | (key, value) <- toList fields]

    addAuth =
      maybe id \ (HackageUser user, HackagePassword password) ->
        applyBasicAuth (encodeUtf8 user) (encodeUtf8 password)

    addQuery = maybe id \ q -> setQueryString (second Just <$> toList q)

makeNativeRequest ::
  ∀ a .
  HackageResources ->
  HackageRequest a ->
  M (Either HackageError a)
makeNativeRequest HackageResources {manager, location} request = do
  appContextDebug [exon|sending request to Hackage at #{Color.url location.host} (#{request.path})|] do
    nrequest <- nativeRequest location request
    response <- liftIO (httpLbs nrequest manager)
    let
      status = responseStatus response

      errorStatus category = requestFailed [exon|#{category} (#{decodeUtf8 (statusMessage status)})|]

      result = if
        | statusIsSuccessful status -> parseResult (responseBody response) request.accept
        | statusCode status == 404 -> Left HackageNotFound
        | statusIsClientError status -> errorStatus "Client error"
        | statusIsServerError status -> errorStatus "Server error"
        | otherwise -> errorStatus "Weird error"

    checkResult result
    pure result
  where
    requestFailed = Left . HackageFatal

    checkResult = \case
      Left (HackageFatal msg) -> Log.error [exon|Hackage request for '#{request.path}' failed: #{msg}|]
      _ -> unit

handlersProd :: HackageResources -> HackageClient
handlersProd resources =
  HackageClient {
    description = resources.description,
    request = makeNativeRequest resources
  }

handlersProdFor ::
  Maybe Manager ->
  HackagePurpose ->
  CabalConfig ->
  M (NonEmpty HackageClient)
handlersProdFor sharedManager purpose conf = do
  manager <- fromMaybeA httpManager sharedManager
  matching <- hackagesFor purpose conf
  pure (repoHandlers manager <$> matching)
  where
    repoHandlers manager HackageRepo {description, location} =
      handlersProd (HackageResources {manager, description, location})

mockUser :: NonEmpty (Text, Text)
mockUser =
  [
    ("username", "test"),
    ("password", "test"),
    ("repeat-password", "test")
  ]

handlersMock :: Manager -> Port -> M HackageClient
handlersMock manager port = do
  _ <- hackagePostQuery Right adminClient "users/" mockUser HackageNoResponse
  _ <- hackagePut @Value Right adminClient "packages/uploaders/user/test"
  pure (handlersProd userRes)
  where
    res = HackageResources {
      manager,
      description = "mock Hackage",
      location = HackageLocation {
        host = "localhost",
        tls = TlsOff,
        port = Just (fromIntegral port),
        auth = Just ("admin", "admin")
      }
    }
    userRes = res {location = res.location {auth = Just ("test", "test")}}
    adminClient = handlersProd res
