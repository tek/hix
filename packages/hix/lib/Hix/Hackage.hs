module Hix.Hackage where

import Control.Monad.Extra (fromMaybeM)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', withObject, (.:))
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import Distribution.Parsec (eitherParsec)
import Hix.Data.Version (Version)
import Exon (exon)
import Network.HTTP.Client (Manager, Request (..), Response (..), defaultRequest, httpLbs)
import Network.HTTP.Types (
  Status (statusCode, statusMessage),
  hAccept,
  statusIsClientError,
  statusIsServerError,
  statusIsSuccessful,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc, readProcess)

import Hix.Data.Error (Error (Fatal))
import Hix.Data.PackageId (PackageId, renderPackage)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (SourceHash (SourceHash))
import qualified Hix.Log as Log
import Hix.Monad (M, throwM, tryIOM)
import Hix.Pretty (showP)

data HackageVersions =
  HackageVersions {
    versions :: [String]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON HackageVersions where
  parseJSON = withObject "HackageVersions" \ o -> HackageVersions <$> o .: "normal-version"

parseVersion :: String -> Either (String, String) Version
parseVersion s = first (s,) (eitherParsec s)

parseResult :: LByteString -> M (Either Text [Version])
parseResult body =
  case eitherDecodeStrict' (toStrict body) of
    Left err ->
      noVersion [exon|Hackage response parse error: #{toText err}|]
    Right (HackageVersions []) ->
      noVersion "No versions on Hackage"
    Right (HackageVersions versions) ->
      case traverse parseVersion versions of
        Left (v, err) -> noVersion (toText [exon|Version '#{v}' has invalid format (#{err})|])
        Right vs -> pure (Right vs)
  where
    noVersion = pure . Left

versionsHackage :: Manager -> PackageName -> M [Version]
versionsHackage manager pkg = do
  res <- liftIO (httpLbs request manager)
  let
    body = responseBody res

    status = responseStatus res

    errorStatus category = noVersion [exon|#{category} (#{decodeUtf8 (statusMessage status)})|]

  if
    | statusIsSuccessful status -> leftA noVersion =<< parseResult body
    | statusCode status == 404 -> noVersion "PackageId does not exist"
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
      [] <$ Log.error [exon|Hackage request for '##{pkg}' failed: #{msg}|]

latestVersionHackage :: Manager -> PackageName -> M (Maybe Version)
latestVersionHackage manager pkg =
  head <$> versionsHackage manager pkg

fetchHashHackage ::
  PackageId ->
  M SourceHash
fetchHashHackage package = do
  Log.debug [exon|Fetching hash for '##{slug}' from ##{url}|]
  tryIOM (readProcess conf) >>= \case
    (ExitFailure _, _, err) ->
      throwM (Fatal [exon|Prefetching source of '##{slug}' from hackage failed: #{decodeUtf8 err}|])
    (ExitSuccess, hash, _) ->
      pure (SourceHash (Text.stripEnd (decodeUtf8 hash)))
  where
    conf = proc "nix-prefetch-url" ["--unpack", url]
    url = [exon|https://hackage.haskell.org/package/#{slug}/#{slug}.tar.gz|]
    slug = showP package

fetchHashHackageCached ::
  IORef (Map Text SourceHash) ->
  PackageId ->
  M SourceHash
fetchHashHackageCached cacheRef package =
  liftIO (readIORef cacheRef) >>= \ cache ->
    fromMaybeM fetch (pure (cache !? cacheKey))
  where
    fetch = do
      hash <- fetchHashHackage package
      hash <$ addToCache hash

    addToCache hash = liftIO (modifyIORef' cacheRef (Map.insert cacheKey hash))

    cacheKey = renderPackage package
