module Hix.Hackage.Hash where

import Control.Monad.Extra (firstJustM, fromMaybeM)
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Text as Text
import Exon (exon)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc, readProcess)

import qualified Hix.Color as Color
import Hix.Data.PackageId (PackageId, renderPackage)
import Hix.Data.Version (SourceHash (SourceHash))
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.HackageRepo (HackageName, HackageRepo (..))
import qualified Hix.Managed.Cabal.HackageLocation as HackageLocation
import Hix.Monad (M, appContext, noteFatal, tryIOM)
import Hix.Pretty (showP)

fetchHashHackageRepo ::
  PackageId ->
  HackageRepo ->
  M (Maybe (SourceHash, HackageName))
fetchHashHackageRepo package HackageRepo {name, location} =
  appContext [exon|trying ##{url}|] do
    tryIOM (readProcess conf) >>= \case
      (ExitFailure _, _, err) -> do
        Log.debug [exon|Error for ##{url}: #{decodeUtf8 err}|]
        pure Nothing
      (ExitSuccess, hash, _) ->
        pure (Just (SourceHash (Text.stripEnd (decodeUtf8 hash)), name))
  where
    conf = proc "nix-prefetch-url" ["--unpack", url]
    url = [exon|#{HackageLocation.renderMinimal location}/package/#{slug}/#{slug}.tar.gz|]
    slug = showP package

fetchHashHackage ::
  NonEmpty HackageRepo ->
  PackageId ->
  M (SourceHash, HackageName)
fetchHashHackage repos package =
  appContext [exon|fetching hash for #{Color.package package} from Hackage repos|] do
    noteFatal notFound =<< firstJustM (fetchHashHackageRepo package) (toList repos)
  where
    notFound = [exon|No Hackage repo knows the package ID #{Color.package package}|]

fetchHashHackageCached ::
  IORef (Map Text (SourceHash, Maybe HackageName)) ->
  NonEmpty HackageRepo ->
  PackageId ->
  M (SourceHash, Maybe HackageName)
fetchHashHackageCached cacheRef repos package =
  liftIO (readIORef cacheRef) >>= \ cache ->
    fromMaybeM fetch (pure (cache !? cacheKey))
  where
    fetch = do
      hash <- second Just <$> fetchHashHackage repos package
      hash <$ addToCache hash

    addToCache hash = liftIO (modifyIORef' cacheRef (Map.insert cacheKey hash))

    cacheKey = renderPackage package
