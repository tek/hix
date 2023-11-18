module Hix.Managed.Handlers.LowerInit.Test where

import Data.IORef (IORef)
import Path (Abs, Dir, Path)

import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Handlers.LowerInit (LowerInitHandlers (..), handlersNull)
import Hix.Managed.Handlers.LowerInit.Prod (handlersProd)
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers
import Hix.Managed.Lower.Data.LowerInit (LowerInit)
import Hix.Data.Monad (M)

handlersTest ::
  Maybe (Path Abs Dir) ->
  Bool ->
  M LowerInitHandlers
handlersTest =
  handlersProd

handlersUnitTest :: IO (LowerInitHandlers, IORef [DepMutation LowerInit])
handlersUnitTest = do
  (report, bumpsRef) <- ReportHandlers.handlersUnitTest
  pure (handlersNull {report}, bumpsRef)
