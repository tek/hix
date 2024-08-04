module Hix.Managed.Handlers.Revision.Prod where

import Hix.Data.Monad (M)
import Hix.Http (httpManager)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForPublish))
import Hix.Managed.Data.RevisionConfig (RevisionConfig (..))
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.Revision (RevisionHandlers (..))
import Hix.Managed.Maint.Git (gitApiRevisionHermetic, gitApiRevisionProd)

handlersProd ::
  RevisionConfig ->
  CabalConfig ->
  M RevisionHandlers
handlersProd config cabal = do
  manager <- httpManager
  publishHackages <- HackageClient.handlersProdFor (Just manager) ForPublish cabal
  pure RevisionHandlers {
    git = if config.ci then gitApiRevisionHermetic config else gitApiRevisionProd config,
    publishHackages
  }
