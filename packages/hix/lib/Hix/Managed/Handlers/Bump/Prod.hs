module Hix.Managed.Handlers.Bump.Prod where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Hix.Hackage (latestVersionHackage)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildOutputsPrefix)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers (BumpHandlers))

handlersProd ::
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  IO BumpHandlers
handlersProd stateFileConf envsConf buildOutputsPrefix oldest = do
  manager <- liftIO (newManager tlsManagerSettings)
  build <- Build.handlersProd stateFileConf envsConf buildOutputsPrefix oldest
  pure BumpHandlers {
    build,
    latestVersion = latestVersionHackage manager
  }
