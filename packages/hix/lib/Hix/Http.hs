module Hix.Http where

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

httpManager :: MonadIO m => m Manager
httpManager = liftIO (newManager tlsManagerSettings)
