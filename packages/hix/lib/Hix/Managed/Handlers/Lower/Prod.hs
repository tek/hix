module Hix.Managed.Handlers.Lower.Prod where

import Distribution.Client.Dependency (PackagesPreferenceDefault (PreferAllOldest), setPreferenceDefault)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Hix.Data.ManagedEnv (BuildOutputsPrefix, EnvsConfig)
import Hix.Data.Monad (M)
import Hix.Hackage (versionsHackage)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Data.ManagedPackage (ManagedPackages)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers, EnvBuilder)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers (LowerHandlers))
import qualified Hix.Managed.Handlers.Report.Prod as Report
import Hix.Managed.Handlers.Solve (SolveHandlers)
import qualified Hix.Managed.Handlers.Solve.Prod as SolveHandlers

solve ::
  Bool ->
  ManagedPackages ->
  EnvBuilder ->
  M SolveHandlers
solve oldest packages builder = do
  ghcDb <- builder.ghcDb
  SolveHandlers.handlersProd packages solverParams ghcDb
  where
    solverParams | oldest = setPreferenceDefault PreferAllOldest
                 | otherwise = id

handlersProdWith ::
  BuildHandlers ->
  Bool ->
  M LowerHandlers
handlersProdWith build oldest = do
  manager <- liftIO (newManager tlsManagerSettings)
  pure LowerHandlers {
    build,
    solve = solve oldest,
    report = Report.handlersProd,
    versions = versionsHackage manager
  }

handlersProd ::
  StateFileConfig ->
  EnvsConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  M LowerHandlers
handlersProd stateFileConf envsConf buildOutputsPrefix oldest = do
  build <- liftIO (Build.handlersProd stateFileConf envsConf buildOutputsPrefix)
  handlersProdWith build oldest
