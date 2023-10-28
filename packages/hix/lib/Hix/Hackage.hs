module Hix.Hackage where

import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', withObject, (.:))
import Distribution.Parsec (eitherParsec)
import Distribution.Version (Version)
import Exon (exon)
import Network.HTTP.Client (Manager, Request (..), Response (..), defaultRequest, httpLbs)
import Network.HTTP.Types

import Hix.Data.ComponentConfig (PackageName)
import Hix.Data.Error (printError)
import Hix.Monad (M)

data HackageVersions =
  HackageVersions {
    versions :: [String]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON HackageVersions where
  parseJSON = withObject "HackageVersions" \ o -> HackageVersions <$> o .: "normal-version"

parseResult :: PackageName -> LByteString -> M (Maybe Version)
parseResult pkg body =
  case eitherDecodeStrict' (toStrict body) of
    Left err ->
      noVersion [exon|Hackage response parse error: #{toText err}|]
    Right (HackageVersions []) ->
      noVersion "No versions on Hackage"
    Right (HackageVersions (versionString : _)) ->
      case eitherParsec versionString of
        Left err -> noVersion (toText [exon|Version '#{versionString}' has invalid format (#{err})|])
        Right version -> pure (Just version)
  where
    noVersion msg =
      Nothing <$ printError [exon|Finding latest version for '##{pkg}'|] msg

latestVersionHackage :: Manager -> PackageName -> M (Maybe Version)
latestVersionHackage manager pkg = do
  res <- liftIO (httpLbs request manager)
  let
    body = responseBody res

    status = responseStatus res

    errorStatus category = noVersion [exon|#{category} (#{decodeUtf8 (statusMessage status)})|]

  if
    | statusIsSuccessful status -> parseResult pkg body
    | statusCode status == 404 -> noVersion "Package does not exist"
    | statusIsClientError status -> errorStatus "Client error"
    | statusIsServerError status -> errorStatus "Server error"
    | otherwise -> errorStatus "Weird error"

  where
    request =
      defaultRequest {
        host = "hackage.haskell.org",
        secure = False,
        method = "GET",
        path = [exon|/package/##{pkg}/preferred|],
        requestHeaders = [(hAccept, "application/json")]
      }

    noVersion msg =
      Nothing <$ printError [exon|Hackage request for '##{pkg}' failed|] msg
