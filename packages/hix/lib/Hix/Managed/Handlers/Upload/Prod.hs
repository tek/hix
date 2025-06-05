module Hix.Managed.Handlers.Upload.Prod where

import Exon (exon)
import GHC.IsList (fromList)
import Network.URI (uriToString)
import Path (Abs, File, Path)

import qualified Hix.Color as Color
import Hix.Data.Monad (M)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish), hackagesFor)
import Hix.Managed.Cabal.Data.HackageLocation (HackageLocation (..))
import Hix.Managed.Cabal.Data.HackageRepo (HackageName, HackageRepo (..))
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Managed.Cabal.HackageLocation (hackageLocationUri)
import Hix.Managed.Cabal.Init (globalFlagsWithDefaultCacheDir, remoteRepo)
import Hix.Managed.Cabal.Resources (cabalVerbosity)
import qualified Hix.Managed.Cabal.Upload as Upload
import Hix.Managed.Cabal.Upload (UploadConfig (..))
import Hix.Managed.Data.UploadResult (HackageUrl, RepoResult (..), ArtifactResult)
import Hix.Managed.Handlers.Upload (UploadHandlers (..))
import Hix.Monad (appContextVerbose, catchM, noteClient)
import Hix.Pretty (showP)

noCredentialsError :: HackageName -> Text
noCredentialsError name =
  [exon|The Hackage repo #{Color.config name} is marked for uploading but has no credentials|]

uploadConfig :: HackageRepo -> M UploadConfig
uploadConfig HackageRepo {name, location = HackageLocation {auth}} = do
  verbosity <- cabalVerbosity
  (user, password) <- noteClient (noCredentialsError name) auth
  pure UploadConfig {verbosity, user, password}

-- | Generate the base URL for a Hackage server (for package links).
hackagePackageUrl :: HackageLocation -> HackageUrl
hackagePackageUrl loc =
  fromString $ uriToString id (hackageLocationUri loc) "package/"

uploadToRepo ::
  Bool ->
  UploadStage ->
  Path Abs File ->
  HackageRepo ->
  M (HackageName, RepoResult)
uploadToRepo useGlobalConfig stage dist repo =
  appContextVerbose [exon|uploading ##{dist} as #{showP stage} to ##{name}|] do
    catchM (success <$ upload) failure
  where
    name = repo.name
    loc = repo.location

    success = (name, RepoSuccess (hackagePackageUrl loc))

    upload = do
      flags <- globalFlagsWithDefaultCacheDir useGlobalConfig (remoteRepo <$> [repo])
      config <- uploadConfig repo
      Upload.uploadPackage stage config flags dist

    failure err = pure (name, RepoFailed err)

uploadToRepos ::
  Bool ->
  NonEmpty HackageRepo ->
  UploadStage ->
  Path Abs File ->
  M ArtifactResult
uploadToRepos useGlobalConfig repos stage config =
  fromList . toList <$> traverse (uploadToRepo useGlobalConfig stage config) repos

handlersProd :: Bool -> CabalConfig -> M UploadHandlers
handlersProd useGlobalConfig cabal = do
  repos <- hackagesFor ForPublish cabal
  pure UploadHandlers {upload = uploadToRepos useGlobalConfig repos}
