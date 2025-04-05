module Hix.Integration.Utils where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Hedgehog (TestT, evalEither, property, test, withTests, (===))
import Path (Abs, Dir, File, Path, Rel, parent, reldir, toFilePath, (</>))
import Path.IO (createDirIfMissing)
import System.Environment (lookupEnv)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (M)
import Hix.Error (pathText)
import Hix.Managed.Git (GitNative (..))
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

runMTest :: Bool -> M a -> TestT IO a
runMTest debug =
  runMTestLog if debug then def {logLevel = LogDebug} else def

runMLogTest :: LogConfig -> M a -> TestT IO ([Text], a)
runMLogTest logConf prog =
  toTestT (sequence <$> runMLogTestDir logConf prog)

runMTestVerboseCabal :: M a -> TestT IO a
runMTestVerboseCabal ma =
  toTestT (runMTestDirWith (\ o -> o {GlobalOptions.logLevel = LogDebug, GlobalOptions.cabalVerbose = True}) ma)

addFile :: Path Abs Dir -> Path Rel File -> Text -> M ()
addFile root path content = do
  createDirIfMissing True (parent file)
  liftIO (Text.writeFile (toFilePath file) content)
  where
    file = root </> path

add :: GitNative -> Path Rel File -> Text -> M ()
add git path content = do
  addFile git.repo path content
  git.cmd_ ["add", pathText path]

local1 :: Path Rel Dir
local1 = [reldir|packages/local1|]

addP :: GitNative -> Path Rel File -> Text -> M ()
addP git path = add git (local1 </> path)

libHs :: Text
libHs = [exon|module Lib where|]

withHixDir :: (Text -> UnitTest) -> UnitTest
withHixDir main = do
  liftIO (lookupEnv "hix_dir") >>= \case
    Nothing -> unit
    Just hixRoot -> main (toText hixRoot)

eqLines ::
  ∀ m .
  Monad m =>
  HasCallStack =>
  Text ->
  Text ->
  TestT m ()
eqLines l r =
  withFrozenCallStack do
    take common linesL === take common linesR
    if ll > lr
    then trailing "missing lines: " linesL
    else if lr > ll
    then trailing "extra lines: " linesR
    else unit
  where
    trailing desc ls = fail (toString (desc <> Text.unlines (drop common ls)))
    common = min ll lr
    ll = length linesL
    lr = length linesR
    linesL = Text.lines l
    linesR = Text.lines r
