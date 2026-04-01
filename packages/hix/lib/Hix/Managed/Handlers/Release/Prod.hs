module Hix.Managed.Handlers.Release.Prod where

import Data.List.Extra (takeEnd)
import qualified Data.Text as Text

import Hix.Data.Monad (M)
import Hix.Data.Options (ReleaseOptions (..))
import Hix.Http (httpManager)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish))
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import Hix.Managed.Flake (runFlake)
import Hix.Managed.Git (GitApi)
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.Handlers.Release (ReleaseHandlers (..))
import qualified Hix.Managed.Handlers.ReleaseUi.Batch as ReleaseUi
import qualified Hix.Managed.Handlers.ReleaseUi.Prod as ReleaseUi
import qualified Hix.Managed.Handlers.Upload.Prod as Upload
import Hix.Managed.Release.Git (GitExtraArgs, GitRelease, gitApiReleaseHermetic, gitApiReleaseProd)
import Hix.Managed.Release.Package (releaseDist, uploadArtifact)

runChecksProd :: M (Maybe [Text])
runChecksProd =
  fmap truncateLog . leftToMaybe <$> runFlake "Checks" ["-L", "flake", "check"] def (const unit)
  where
    truncateLog = takeEnd 100 . Text.lines

handlersProd ::
  ReleaseOptions ->
  CabalConfig ->
  M ReleaseHandlers
handlersProd options cabal = do
  manager <- httpManager
  project <- Project.handlersProd options.stateFile
  upload <- Upload.handlersProd options.config.globalCabalConfig cabal
  publishHackages <- HackageClient.handlersProdFor (Just manager) ForPublish cabal
  pure ReleaseHandlers {
    runChecks = runChecksProd,
    releaseDist,
    uploadArtifact = \ desc stage -> uploadArtifact desc stage upload,
    git = gitApi options.config,
    context = ContextHandlers.handlersProd,
    project,
    upload,
    publishHackages,
    ui
  }
  where
    gitApi :: ReleaseConfig -> GitExtraArgs -> GitApi GitRelease
    gitApi =
      if options.config.globalGit
      then gitApiReleaseProd
      else gitApiReleaseHermetic

    ui =
      if options.config.interactive
      then ReleaseUi.handlersProdWithEvents options.uiDebug
      else ReleaseUi.handlersBatch options.config
