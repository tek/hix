module Hix.Test.Utils where

import Hedgehog (TestT, property, test, withTests)
import Path (Abs, Dir, Path, absdir)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Hix.Data.Error (Error)
import Hix.Data.GlobalOptions (GlobalOptions (debug, quiet, verbose), defaultGlobalOptions)
import Hix.Monad (M, runMWith)

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

runMTest :: Bool -> M a -> IO (Either Error a)
runMTest debug =
  runMWith (defaultGlobalOptions testRoot) {verbose = debug, debug, quiet = not debug}
