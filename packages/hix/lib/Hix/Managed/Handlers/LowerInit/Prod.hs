module Hix.Managed.Handlers.LowerInit.Prod where

import Distribution.Client.Dependency (PackagesPreferenceDefault (PreferAllOldest), setPreferenceDefault)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Path (Abs, Dir, Path)

import Hix.Hackage (versionsHackage)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.LowerInit
import Hix.Managed.Handlers.LowerInit (LowerInitHandlers (LowerInitHandlers))
import qualified Hix.Managed.Handlers.Report.Prod as Report
import Hix.Data.Monad (M)

handlersProd ::
  Maybe (Path Abs Dir) ->
  Bool ->
  M LowerInitHandlers
handlersProd ghc oldest = do
  manager <- liftIO (newManager tlsManagerSettings)
  build <- Build.handlersProd solverParams ghc
  pure LowerInitHandlers {
    build,
    report = Report.handlersProd,
    versions = versionsHackage manager
  }
  where
    solverParams | oldest = setPreferenceDefault PreferAllOldest
                 | otherwise = id
