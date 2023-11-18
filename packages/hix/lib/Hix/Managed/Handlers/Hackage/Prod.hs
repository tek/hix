module Hix.Managed.Handlers.Hackage.Prod where

import Data.IORef (newIORef)

import Hix.Hackage (fetchHashHackageCached)
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers (HackageHandlers))

handlersProd :: IO HackageHandlers
handlersProd = do
  hashCache <- newIORef mempty
  pure HackageHandlers {
    fetchHash = fetchHashHackageCached hashCache
  }
