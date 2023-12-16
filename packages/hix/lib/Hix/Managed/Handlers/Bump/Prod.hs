module Hix.Managed.Handlers.Bump.Prod where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Hix.Data.ManagedEnv (BuildOutputsPrefix, EnvsConfig)
import Hix.Hackage (latestVersionHackage)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers (BumpHandlers))
import qualified Hix.Managed.Handlers.Report.Prod as Report

handlersProd ::
  StateFileConfig ->
  EnvsConfig ->
  Maybe BuildOutputsPrefix ->
  IO BumpHandlers
handlersProd stateFileConf envsConf buildOutputsPrefix = do
  manager <- liftIO (newManager tlsManagerSettings)
  build <- Build.handlersProd stateFileConf envsConf buildOutputsPrefix
  pure BumpHandlers {
    build,
    report = Report.handlersProd,
    latestVersion = latestVersionHackage manager
  }
