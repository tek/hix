module Hix.Managed.Handlers.Lower.Test where

import Data.IORef (IORef)

import Hix.Data.ManagedEnv (BuildOutputsPrefix)
import Hix.Data.Monad (M)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import Hix.Managed.Handlers.Lower (LowerHandlers (..), handlersNull)
import Hix.Managed.Handlers.Lower.Prod (handlersProd)
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers
import Hix.Managed.Lower.Data.Lower (Lower)

handlersTest ::
  StateFileConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  M LowerHandlers
handlersTest =
  handlersProd

handlersUnitTest :: IO (LowerHandlers, IORef [DepMutation Lower])
handlersUnitTest = do
  (report, bumpsRef) <- ReportHandlers.handlersUnitTest
  pure (handlersNull {report}, bumpsRef)
