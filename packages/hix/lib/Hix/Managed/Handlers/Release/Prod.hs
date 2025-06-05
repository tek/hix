module Hix.Managed.Handlers.Release.Prod where

import Data.List.Extra (takeEnd)
import qualified Data.Text as Text
import Exon (exon)

import qualified Hix.Color as Color
import Hix.Data.Monad (LogLevel (..), M)
import Hix.Data.Options (ReleaseOptions (..))
import Hix.Http (httpManager)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish))
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import Hix.Managed.Flake (runFlake)
import qualified Hix.Managed.Handlers.Context as ContextHandlers
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import qualified Hix.Managed.Handlers.Project.Prod as Project
import Hix.Managed.Handlers.Release (ReleaseHandlers (..))
import qualified Hix.Managed.Handlers.ReleaseUi.Batch as ReleaseUi
import qualified Hix.Managed.Handlers.ReleaseUi.Prod as ReleaseUi
import qualified Hix.Managed.Handlers.Upload.Prod as Upload
import Hix.Managed.Git (GitApi)
import Hix.Managed.Release.Git (GitExtraArgs, GitRelease, gitApiReleaseHermetic, gitApiReleaseProd)
import Hix.Managed.Release.Package (releaseDist, uploadArtifact)
import Hix.Monad (shouldLog)

checksFailed :: Text -> M Bool
checksFailed output = do
  Log.error "The flake checks failed."
  ifM (shouldLog LogVerbose)
    do
      Log.verbose "Last 100 lines of output:"
      traverse_ Log.cont.verbose (takeEnd 100 (Text.lines output))
    do
      Log.error [exon|Run #{col "nix -L flake check"} manually or re-run this command with #{col "--verbose"}|]
  pure False
  where
    col = Color.shellCommand @Text

runChecksProd :: M Bool
runChecksProd =
  either checksFailed (const (pure True)) =<< runFlake "Checks" ["-L", "flake", "check"] def (const unit)

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
    uploadArtifact = \desc stage package dist -> uploadArtifact desc stage upload package dist,
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
