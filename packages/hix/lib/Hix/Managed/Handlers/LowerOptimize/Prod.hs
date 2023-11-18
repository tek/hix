module Hix.Managed.Handlers.LowerOptimize.Prod where

import Distribution.Client.Dependency (PackagesPreferenceDefault (PreferAllOldest), setPreferenceDefault)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Path (Abs, Dir, Path)

import Hix.Hackage (versionsHackage)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.LowerOptimize
import Hix.Managed.Handlers.LowerOptimize (LowerOptimizeHandlers (LowerOptimizeHandlers))
import qualified Hix.Managed.Handlers.Report.Prod as Report
import Hix.Monad (M)

handlersProd ::
  Maybe (Path Abs Dir) ->
  Bool ->
  M LowerOptimizeHandlers
handlersProd ghc oldest = do
  manager <- liftIO (newManager tlsManagerSettings)
  build <- Build.handlersProd solverParams ghc
  pure LowerOptimizeHandlers {
    build,
    report = Report.handlersProd,
    versions = versionsHackage manager
  }
  where
    solverParams | oldest = setPreferenceDefault PreferAllOldest
                 | otherwise = id
