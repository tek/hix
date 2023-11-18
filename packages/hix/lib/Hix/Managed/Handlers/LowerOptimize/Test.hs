module Hix.Managed.Handlers.LowerOptimize.Test where

import Data.IORef (IORef)
import Path (Abs, Dir, Path)

import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Handlers.LowerOptimize (LowerOptimizeHandlers (..), handlersNull)
import Hix.Managed.Handlers.LowerOptimize.Prod (handlersProd)
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize)
import Hix.Monad (M)

handlersTest ::
  Maybe (Path Abs Dir) ->
  Bool ->
  M LowerOptimizeHandlers
handlersTest =
  handlersProd

handlersUnitTest :: IO (LowerOptimizeHandlers, IORef [DepMutation LowerOptimize])
handlersUnitTest = do
  (report, bumpsRef) <- ReportHandlers.handlersUnitTest
  pure (handlersNull {report}, bumpsRef)
