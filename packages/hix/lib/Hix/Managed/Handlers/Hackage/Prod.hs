module Hix.Managed.Handlers.Hackage.Prod where

import Data.IORef (newIORef)

import Hix.Data.Version (SourceHash)
import Hix.Hackage (fetchHashHackageCached)
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers (HackageHandlers))

handlersProdWith ::
  MonadIO m =>
  Map Text SourceHash ->
  m HackageHandlers
handlersProdWith initial = do
  hashCache <- liftIO (newIORef initial)
  pure HackageHandlers {
    fetchHash = fetchHashHackageCached hashCache
  }

handlersProd ::
  MonadIO m =>
  m HackageHandlers
handlersProd = handlersProdWith mempty
