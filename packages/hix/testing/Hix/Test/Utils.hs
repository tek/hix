module Hix.Test.Utils where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import qualified Data.Text.IO as Text
import Hedgehog (TestT, evalEither, property, test, withTests)
import Hedgehog.Internal.Property (Failure, Journal, TestT (..))
import Path (Abs, Dir, File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Data.Error (Error)
import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (M)
import Hix.Monad.Run (runMWith)
import Hix.Test.Hedgehog (TestTCompat (..), runTestTCompat)
import Hix.Test.Managed.UnsafeIsString ()
import Hix.Test.Run (LogConfig (..), runMLogTestDir, runMTestDir, runMTestDirWith, withTestDir)

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

runMTestWith :: (GlobalOptions -> GlobalOptions) -> M a -> TestT IO a
runMTestWith f ma =
  toTestT (runMTestDirWith f ma)

runMTest :: Bool -> M a -> TestT IO a
runMTest debug =
  runMTestLog if debug then def {logLevel = LogDebug} else def

runMLogTest :: LogConfig -> M a -> TestT IO ([Text], a)
runMLogTest logConf prog =
  toTestT (sequence <$> runMLogTestDir logConf prog)

runMTestVerboseCabal :: M a -> TestT IO a
runMTestVerboseCabal =
  runMTestWith \ o -> o {GlobalOptions.logLevel = LogDebug, GlobalOptions.cabalVerbose = True}

addFile :: Path Abs Dir -> Path Rel File -> Text -> M ()
addFile root path content = do
  createDirIfMissing True (parent file)
  liftIO (Text.writeFile (toFilePath file) content)
  where
    file = root </> path

withLowerTestT ::
  Monad n =>
  ((∀ m x . TestT m x -> m (Either Failure x, Journal)) -> n (Either Failure a, Journal)) ->
  TestT n a
withLowerTestT use = do
  (result, journal) <- TestT (lift (lift (use lower)))
  TestT $ lift $ tell journal
  case result of
    Left failure -> TestT (ExceptT (pure (Left failure)))
    Right a -> pure a
  where
    lower (TestT t) = runWriterT (runExceptT t)

runTestTMTestDir ::
  (GlobalOptions -> M (Either Failure a, Journal) -> IO (Either Error (Either Failure a, Journal))) ->
  TestT M a ->
  TestT IO a
runTestTMTestDir run prog =
  runTestTCompat do
    withTestDir \ options ->
      TestTCompat do
        result <- withLowerTestT \ lower -> do
          run options (lower prog) <&> \case
            Left err -> (Right (Left err), mempty)
            Right (res, journal) -> (Right <$> res, journal)
        evalEither result

runTestTMWith :: (GlobalOptions -> GlobalOptions) -> TestT M a -> TestT IO a
runTestTMWith f =
  runTestTMTestDir \ options -> runMWith (f options)

runTestTMLog :: LogConfig -> TestT M a -> TestT IO a
runTestTMLog LogConfig {logLevel} =
  runTestTMWith \ options -> options {GlobalOptions.logLevel}

runTestTM :: Bool -> TestT M a -> TestT IO a
runTestTM debug =
  runTestTMLog if debug then def {logLevel = LogDebug} else def
