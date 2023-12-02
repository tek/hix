module Hix.Managed.Handlers.Lower.Prod where

import Distribution.Client.Dependency (PackagesPreferenceDefault (PreferAllOldest), setPreferenceDefault)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Path (Abs, Dir, Path)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Hackage (versionsHackage)
import Hix.Managed.Build.Mutation (RenderMutation)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Build.Prod as Build
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers (LowerHandlers))
import qualified Hix.Managed.Handlers.Report.Prod as Report
import Hix.Managed.Handlers.Solve (SolveHandlers)
import qualified Hix.Managed.Handlers.Solve.Prod as SolveHandlers

solve :: BuildHandlers -> Bool -> Path Abs Dir -> EnvName -> M SolveHandlers
solve build oldest root env = do
  ghc <- build.ghcDb root env
  SolveHandlers.handlersProd solverParams ghc
  where
    solverParams | oldest = setPreferenceDefault PreferAllOldest
                 | otherwise = id

handlersProdWith ::
  RenderMutation s =>
  BuildHandlers ->
  Bool ->
  M (LowerHandlers s)
handlersProdWith build oldest = do
  manager <- liftIO (newManager tlsManagerSettings)
  pure LowerHandlers {
    build,
    solve = solve build oldest,
    report = Report.handlersProd,
    versions = versionsHackage manager
  }

handlersProd ::
  RenderMutation s =>
  Maybe Text ->
  Bool ->
  M (LowerHandlers s)
handlersProd buildOutputsPrefix oldest = do
  build <- liftIO (Build.handlersProd buildOutputsPrefix)
  handlersProdWith build oldest
