module Hix.Test.Utils where

import Hedgehog (TestT, evalEither, property, test, withTests)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (M)
import Hix.Test.Managed.UnsafeIsString ()
import Hix.Test.Run (LogConfig (logLevel), runMLogTestDir, runMTestDir)

type UnitTest = TestT IO ()

unitTest ::
  HasCallStack =>
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc t =
  withFrozenCallStack do
    testProperty desc (withTests 1 (property (test t)))

toTestT :: Show e => IO (Either e a) -> TestT IO a
toTestT = evalEither <=< liftIO

runMTestLog :: LogConfig -> M a -> TestT IO a
runMTestLog logConfig ma =
  toTestT (runMTestDir logConfig ma)

runMTest :: Bool -> M a -> TestT IO a
runMTest debug =
  runMTestLog if debug then def {logLevel = LogDebug} else def

runMLogTest :: LogConfig -> M a -> TestT IO ([Text], a)
runMLogTest logConf prog =
  toTestT (sequence <$> runMLogTestDir logConf prog)
