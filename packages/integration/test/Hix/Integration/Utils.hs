module Hix.Integration.Utils where

import qualified Data.Text.IO as Text
import Hedgehog (TestT, evalEither, property, test, withTests)
import Path (Abs, Dir, File, Path, Rel, absdir, parent, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import qualified Hix.Console as Console
import Hix.Data.Error (Error)
import Hix.Data.GlobalOptions (GlobalOptions (..), defaultGlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Monad (M, runMLoggerWith, runMWith, withLogIORef)

type UnitTest = TestT IO ()

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

runMTestWith :: (GlobalOptions -> GlobalOptions) -> M a -> IO (Either Error a)
runMTestWith f =
  runMWith (f (defaultGlobalOptions testRoot))

runMTest' :: Bool -> M a -> IO (Either Error a)
runMTest' debug =
  runMTestWith \ o -> o {logLevel = if debug then LogDebug else LogError}

runMTest :: Bool -> M a -> TestT IO a
runMTest debug ma =
  evalEither =<< liftIO (runMTest' debug ma)

runMTestVerboseCabal :: M a -> TestT IO a
runMTestVerboseCabal ma =
  evalEither =<< liftIO (runMTestWith (\ o -> o {logLevel = LogDebug, cabalVerbose = True}) ma)

runMLogTest :: Bool -> Bool -> M a -> IO ([Text], Either Error a)
runMLogTest debug onlyRef ma =
  withLogIORef \ logger -> do
    let
      fullLogger | onlyRef = logger
                 | otherwise = \ level msg -> do
                   logger level msg
                   Console.err msg
    runMLoggerWith fullLogger opts ma
  where
    opts = (defaultGlobalOptions testRoot) {logLevel = if debug then LogDebug else LogError}

addFile :: Path Abs Dir -> Path Rel File -> Text -> M ()
addFile root path content = do
  createDirIfMissing True (parent file)
  liftIO (Text.writeFile (toFilePath file) content)
  where
    file = root </> path
