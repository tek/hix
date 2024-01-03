module Hix.Managed.Handlers.Hackage.Prod where

import Data.IORef (newIORef)

import Hix.Hackage (fetchHashHackageCached)
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers (HackageHandlers))

handlersProd ::
  MonadIO m =>
  m HackageHandlers
handlersProd = do
  hashCache <- liftIO (newIORef mempty)
  pure HackageHandlers {
    fetchHash = fetchHashHackageCached hashCache
  }
