module Hix.Managed.Handlers.Lower.Prod where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Hix.Data.Monad (M)
import Hix.Hackage (versionsHackage)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildOutputsPrefix)
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers (LowerHandlers))

handlersProdWith ::
  BuildHandlers ->
  M LowerHandlers
handlersProdWith build = do
  manager <- liftIO (newManager tlsManagerSettings)
  pure LowerHandlers {
    build,
    versions = versionsHackage manager
  }

handlersProd ::
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  M LowerHandlers
handlersProd stateFileConf envsConf buildOutputsPrefix oldest = do
  build <- liftIO (Build.handlersProd stateFileConf envsConf buildOutputsPrefix oldest)
  handlersProdWith build
