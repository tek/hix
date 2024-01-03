module Hix.Managed.Handlers.Build.Test where

import Data.IORef (IORef)

import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Data.Version (Versions)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Mutation (FailedMutation)
import Hix.Managed.Data.StageState (BuildStatus)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildHandlers (..), BuildOutputsPrefix, handlersNull, versionsBuilder)
import Hix.Managed.Handlers.Build.Prod (handlersProd)
import qualified Hix.Managed.Handlers.Cabal.Prod as CabalHandlers
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers
import qualified Hix.Managed.Handlers.StateFile.Test as StateFile

handlersUnitTest ::
  MonadIO m =>
  (Versions -> M BuildStatus) ->
  m (BuildHandlers, IORef [Expr], IORef [FailedMutation])
handlersUnitTest builder = do
  (stateFile, stateFileRef) <- StateFile.handlersUnitTest
  (report, mutationsRef) <- ReportHandlers.handlersUnitTest
  pure (handlersNull {stateFile, report, cabal, withBuilder = versionsBuilder builder}, stateFileRef, mutationsRef)
  where
    cabal = CabalHandlers.handlersProd False

handlersTest ::
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  IO BuildHandlers
handlersTest stateFileConf envsConf buildOutputsPrefix oldest = do
  handlers <- handlersProd stateFileConf envsConf buildOutputsPrefix oldest
  pure handlers {cabal = CabalHandlers.handlersTest oldest}
