module Hix.Test.Utils where

import Hedgehog (TestT, evalEither, property, test, withTests)
import Path (Abs, Dir, Path, absdir, reldir, (</>))
import Path.IO (createDirIfMissing, withSystemTempDir)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import qualified Hix.Console as Console
import Hix.Data.Error (Error)
import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions, defaultGlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Monad (M, runMLoggerWith, runMWith, withLogIORef)
import Hix.Test.Managed.UnsafeIsString ()

type UnitTest = TestT IO ()

data LogConfig =
  LogConfig {
    logLevel :: LogLevel,
    refLevel :: LogLevel,
    onlyRef :: Bool
  }
  deriving stock (Eq, Show)

instance Default LogConfig where
  def = LogConfig {logLevel = LogError, refLevel = LogInfo, onlyRef = True}

logConfigDebug :: LogConfig
logConfigDebug = def {logLevel = LogDebug}

logConfigTrace :: LogConfig
logConfigTrace = def {logLevel = LogTrace}

unitTest ::
  HasCallStack =>
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc t =
  withFrozenCallStack do
    testProperty desc (withTests 1 (property (test t)))

testRoot :: Path Abs Dir
testRoot = [absdir|/project|]

withTestDir :: (GlobalOptions -> IO a) -> IO a
withTestDir prog =
  withSystemTempDir "hix-test" \ cwd -> do
    let root = cwd </> [reldir|project|]
    createDirIfMissing False root
    prog (defaultGlobalOptions cwd) {GlobalOptions.root}

runMTest' :: LogConfig -> M a -> IO (Either Error a)
runMTest' LogConfig {logLevel} prog =
  withTestDir \ options ->
    runMWith options {GlobalOptions.logLevel} prog

runMTestLog :: LogConfig -> M a -> TestT IO a
runMTestLog logConfig ma =
  evalEither =<< liftIO (runMTest' logConfig ma)

runMTest :: Bool -> M a -> TestT IO a
runMTest debug =
  runMTestLog if debug then def {logLevel = LogDebug} else def

runMLogTest :: LogConfig -> M a -> IO ([Text], Either Error a)
runMLogTest LogConfig {..} ma =
  withLogIORef \ logger ->
    withTestDir \ options ->
      runMLoggerWith (fullLogger logger) options {GlobalOptions.logLevel = max logLevel refLevel} ma
  where
    fullLogger logger | onlyRef = logger
                      | otherwise = \ level msg -> do
                        when (level <= refLevel) do
                          logger level msg
                        when (level <= logLevel) do
                          Console.err msg
