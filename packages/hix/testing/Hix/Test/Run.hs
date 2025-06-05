module Hix.Test.Run where

import Control.Monad.Catch (MonadMask)
import Distribution.Compat.Lens ((.~))
import Path (Abs, Dir, Path, SomeBase (Abs), absdir, reldir, (</>))
import Path.IO (createDirIfMissing, withSystemTempDir)

import qualified Hix.Console as Console
import Hix.Data.Error (Error)
import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions, defaultGlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (M)
import Hix.Data.PathSpec (PathSpec (PathConcrete))
import Hix.Handlers.Tui (TuiHandlers (..))
import Hix.Handlers.Tui.InMemory (withTuiInMemory)
import qualified Hix.Handlers.Tui.Prod as Tui
import Hix.Monad.Run (runMTuiWith, runMWith)

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

withTestDir ::
  MonadIO m =>
  MonadMask m =>
  (GlobalOptions -> m a) ->
  m a
withTestDir prog =
  withSystemTempDir "hix-test" \ cwd -> do
    let root = cwd </> [reldir|project|]
    createDirIfMissing False root
    prog (defaultGlobalOptions cwd) {GlobalOptions.root = PathConcrete $ Abs root}

runMTestDir :: LogConfig -> M a -> IO (Either Error a)
runMTestDir LogConfig {logLevel} prog =
  withTestDir \ options ->
    runMWith options {GlobalOptions.logLevel} prog

runMTestDirWith :: (GlobalOptions -> GlobalOptions) -> M a -> IO (Either Error a)
runMTestDirWith f prog =
  withTestDir \ options ->
    runMWith (f options) prog

runMLogTestDir :: LogConfig -> M a -> IO ([Text], Either Error a)
runMLogTestDir LogConfig {..} ma = do
  base <- Tui.handlersProd
  withTuiInMemory base \ tui ->
    withTestDir \ options ->
      runMTuiWith (fullLogger tui) options {GlobalOptions.logLevel = max logLevel refLevel} ma
  where
    fullLogger tui
      | onlyRef
      = tui
      | otherwise
      = tui & #log .~ logBoth tui

    logBoth tui level msg = do
      when (level <= refLevel) do
        tui.log level msg
      when (level <= logLevel) do
        Console.err msg
