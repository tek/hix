module Hix.Managed.Handlers.Report.Test where

import Data.IORef (IORef, modifyIORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (BuildResults)
import Hix.Managed.Handlers.Report (ReportHandlers (..), handlersNull)

reportMutationsIORef ::
  IORef [DepMutation a] ->
  BuildResults a ->
  M ()
reportMutationsIORef out results =
  liftIO $ modifyIORef out \ old -> foldl' (\ z res -> res.failed ++ z) old (toList results.envs)

handlersUnitTest :: IO (ReportHandlers a, IORef [DepMutation a])
handlersUnitTest = do
  bumpsRef <- newIORef []
  pure (handlersNull {mutations = reportMutationsIORef bumpsRef}, bumpsRef)
