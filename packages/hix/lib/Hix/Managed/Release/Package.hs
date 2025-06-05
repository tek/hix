module Hix.Managed.Release.Package where

import Data.Aeson (FromJSON)
import Exon (exon)
import Path (Abs, Dir, File, Path)
import Path.IO (listDir)

import Hix.Class.Map ((!?))
import qualified Hix.Color as Color
import Hix.Data.Error (Error)
import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.PackageName (LocalPackage)
import Hix.Error (pathText)
import Hix.Managed.Cabal.Data.UploadStage (ArtifactSort (..), UploadStage (..))
import Hix.Managed.Data.UploadResult (ArtifactResult)
import Hix.Managed.Flake (runFlakeJson)
import Hix.Managed.Handlers.Upload (UploadHandlers (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseDist (..))
import Hix.Managed.Release.Data.Staged (SelectedTargetView (..))
import Hix.Monad (appContext, fatalError, noteFatal, tryIOM, tryM)

data NixOutputs =
  NixOutputs {
    drvPath :: Path Abs File,
    outputs :: Map Text (Path Abs Dir)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

flakeDist ::
  Text ->
  Text ->
  LocalPackage ->
  M (Path Abs File)
flakeDist distType output package = do
  root <- appRes.root
  result <- runFlakeJson @[NixOutputs] desc ["build", "--json", "--no-link", [exon|#{pathText root}#__hix-internal__.dev.##{package}.release.#{output}|]]
  distDir <- noteFatal "No results" do
    NixOutputs {outputs} <- head result
    outputs !? output
  tryIOM (listDir distDir) >>= \case
    (_, [targz]) -> pure targz
    _ -> fatalError [exon|Release derivation's '#{output}' directory contains multiple entries|]
  where
    desc = [exon|#{distType} distribution for ##{package}|]

flakeSourceDist :: LocalPackage -> M (Path Abs File)
flakeSourceDist = flakeDist "Source" "sdist"

flakeDocsDist :: LocalPackage -> M (Path Abs File)
flakeDocsDist = flakeDist "Docs" "haddock"

releaseDist :: SelectedTargetView -> M (Either Error ReleaseDist)
releaseDist target =
  tryM do
    sources <- flakeSourceDist target.package
    docs <- flakeDocsDist target.package
    pure ReleaseDist {..}

-- | Upload an artifact (sources or docs) to all configured Hackage servers.
-- Returns results with both successful uploads (with URLs) and any failures.
uploadArtifact ::
  Text ->
  UploadStage ->
  UploadHandlers ->
  LocalPackage ->
  ReleaseDist ->
  M ArtifactResult
uploadArtifact desc stage handlers package dist =
  appContext [exon|uploading #{desc} for #{Color.package package}|] do
    handlers.upload stage tar
  where
    tar = case stage.artifact of
      ArtifactDocs -> dist.docs
      ArtifactSources -> dist.sources
