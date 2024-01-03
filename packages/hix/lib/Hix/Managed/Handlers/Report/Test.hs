module Hix.Managed.Handlers.Report.Test where

import Data.IORef (IORef, modifyIORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Managed.Data.Mutation (FailedMutation)
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Data.ProjectResult (ProjectResult)
import qualified Hix.Managed.EnvResult as EnvResult
import Hix.Managed.Handlers.Report (ReportHandlers (..), handlersNull)

reportMutationsIORef ::
  IORef [FailedMutation] ->
  ProjectResult ->
  M ()
reportMutationsIORef out results =
  liftIO $ modifyIORef out \ old ->
    foldl' (flip (++)) old failed
  where
    failed = EnvResult.failures <$> toList results.envs

handlersUnitTest ::
  MonadIO m =>
  m (ReportHandlers, IORef [FailedMutation])
handlersUnitTest = do
  mutationsRef <- liftIO (newIORef [])
  pure (handlersNull {mutations = reportMutationsIORef mutationsRef}, mutationsRef)
