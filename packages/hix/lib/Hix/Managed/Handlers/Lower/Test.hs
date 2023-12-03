module Hix.Managed.Handlers.Lower.Test where

import Data.IORef (IORef)

import Hix.Data.Monad (M)
import Hix.Managed.Build.Mutation (DepMutation, RenderMutation)
import Hix.Managed.Handlers.Lower (LowerHandlers (..), handlersNull)
import Hix.Managed.Handlers.Lower.Prod (handlersProd)
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers

handlersTest ::
  RenderMutation a =>
  Maybe Text ->
  Bool ->
  M (LowerHandlers a)
handlersTest =
  handlersProd

handlersUnitTest :: IO (LowerHandlers a, IORef [DepMutation a])
handlersUnitTest = do
  (report, bumpsRef) <- ReportHandlers.handlersUnitTest
  pure (handlersNull {report}, bumpsRef)
