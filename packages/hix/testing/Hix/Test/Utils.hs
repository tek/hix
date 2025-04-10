module Hix.Test.Utils where

import qualified Data.Text.IO as Text
import Hedgehog (TestT, evalEither, property, test, withTests)
import Path (Abs, Dir, File, Path, Rel, parent, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (M)
import Hix.Test.Managed.UnsafeIsString ()
import Hix.Test.Run (LogConfig (logLevel), runMLogTestDir, runMTestDir, runMTestDirWith)

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
