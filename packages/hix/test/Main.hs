module Main where

import Hedgehog (TestT, property, test, withTests)
import Hix.Test.CabalTest (test_cabal)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

unitTest ::
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "parse cabal file" test_cabal
  ]

main :: IO ()
main =
  defaultMain tests
