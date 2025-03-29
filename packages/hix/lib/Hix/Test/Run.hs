module Hix.Test.Run where

import Path (Abs, Dir, Path, absdir, reldir, (</>))
import Path.IO (createDirIfMissing, withSystemTempDir)

import qualified Hix.Console as Console
import Hix.Data.Error (Error)
import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions, defaultGlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Monad (M, runMLoggerWith, runMWith, withLogIORef)

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

testRoot :: Path Abs Dir
testRoot = [absdir|/project|]

withTestDir :: (GlobalOptions -> IO a) -> IO a
withTestDir prog =
  withSystemTempDir "hix-test" \ cwd -> do
    let root = cwd </> [reldir|project|]
    createDirIfMissing False root
    prog (defaultGlobalOptions cwd) {GlobalOptions.root}

runMTestDir :: LogConfig -> M a -> IO (Either Error a)
runMTestDir LogConfig {logLevel} prog =
  withTestDir \ options ->
    runMWith options {GlobalOptions.logLevel} prog

runMTestDirWith :: (GlobalOptions -> GlobalOptions) -> M a -> IO (Either Error a)
runMTestDirWith f prog =
  withTestDir \ options ->
    runMWith (f options) prog

runMLogTestDir :: LogConfig -> M a -> IO ([Text], Either Error a)
runMLogTestDir LogConfig {..} ma =
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
