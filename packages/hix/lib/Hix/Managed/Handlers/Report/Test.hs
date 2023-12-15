module Hix.Managed.Handlers.Report.Test where

import Data.IORef (IORef, modifyIORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.BuildResult (failedMutations)
import qualified Hix.Managed.Data.BuildResults
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Handlers.Report (ReportHandlers (..), handlersNull)

reportMutationsIORef ::
  IORef [DepMutation a] ->
  BuildResults a ->
  M ()
reportMutationsIORef out results =
  liftIO $ modifyIORef out \ old -> foldl' (\ z res -> failedMutations res ++ z) old (toList results.envs)

handlersUnitTest :: IO (ReportHandlers a, IORef [DepMutation a])
handlersUnitTest = do
  mutationsRef <- newIORef []
  pure (handlersNull {mutations = reportMutationsIORef mutationsRef}, mutationsRef)
