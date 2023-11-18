module Hix.Managed.Handlers.Report.Test where

import Data.IORef (IORef, modifyIORef, newIORef)

import Hix.Data.Bounds (UninitializedBounds)
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (BuildResult)
import Hix.Managed.Handlers.Report (ReportHandlers (..), handlersNull)
import Hix.Monad (M)

reportMutationsIORef ::
  IORef [DepMutation a] ->
  UninitializedBounds ->
  BuildResult a ->
  M ()
reportMutationsIORef out _ results =
  liftIO $ modifyIORef out (results.failed ++)

handlersUnitTest :: IO (ReportHandlers a, IORef [DepMutation a])
handlersUnitTest = do
  bumpsRef <- newIORef []
  pure (handlersNull {mutations = reportMutationsIORef bumpsRef}, bumpsRef)
