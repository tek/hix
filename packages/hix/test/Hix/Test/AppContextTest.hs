module Hix.Test.AppContextTest where

import qualified Data.Text as Text
import Exon (exon)
import Hedgehog ((===))

import Hix.Data.LogLevel (LogLevel (..))
import Hix.Error (printErrorWith)
import qualified Hix.Log as Log
import Hix.Monad (appContext, appContextAt, catchingM, fatalError, noteFatal, appContextVerbose)
import Hix.Test.Run (LogConfig (..))
import Hix.Test.Utils (UnitTest, runMLogTest)

target_appContext :: [Text]
target_appContext =
  Text.lines [exon|[34m[1m>>>[0m In context3
[31m[1m>>>[0m While in context1
[31m[1m>>>[0m While in context3
[31m[1m>>>[0m While in context4
[31m[1m>>>[0m Error: Fatal: error
|]

test_appContext :: UnitTest
test_appContext = do
  (log, ()) <- runMLogTest def {refLevel = LogVerbose} do
    caught <- catchingM (pure . Just) do
      appContextAt LogTrace LogError "in context1" do
        appContextAt LogTrace LogDebug "in context2" do
          appContext "in context3" do
            appContextVerbose "in context4" do
              fatalError "error"
    err <- noteFatal "no error" caught
    printErrorWith Log.infoPlain LogVerbose err
  target_appContext === reverse log
