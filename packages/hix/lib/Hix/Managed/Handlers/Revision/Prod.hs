module Hix.Managed.Handlers.Revision.Prod where

import Hix.Data.Monad (M)
import Hix.Http (httpManager)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish))
import Hix.Managed.Data.GitConfig (GitConfig)
import Hix.Managed.Data.RevisionConfig (RevisionConfig (..))
import Hix.Managed.Git (gitApiFromConfigM)
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.Revision (RevisionHandlers (..))
import Hix.Managed.Maint.Git (gitRevisionNative)

handlersProd ::
  RevisionConfig ->
  CabalConfig ->
  Maybe GitConfig ->
  M RevisionHandlers
handlersProd config cabal git = do
  manager <- httpManager
  publishHackages <- HackageClient.handlersProdFor (Just manager) ForPublish cabal
  pure RevisionHandlers {
    git = gitApiFromConfigM git (gitRevisionNative config),
    publishHackages
  }
