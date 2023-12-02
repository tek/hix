module Hix.Managed.Handlers.Bump.Prod where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Hix.Hackage (latestVersionHackage)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Bump
import Hix.Managed.Handlers.Bump (BumpHandlers (BumpHandlers))
import qualified Hix.Managed.Handlers.Report.Prod as Report

handlersProd ::
  Maybe Text ->
  IO BumpHandlers
handlersProd buildOutputsPrefix = do
  manager <- liftIO (newManager tlsManagerSettings)
  build <- Build.handlersProd buildOutputsPrefix
  pure BumpHandlers {
    build,
    report = Report.handlersProd,
    latestVersion = latestVersionHackage manager
  }
