module Main where

import Root.Lib (test)
import Testing (testing)
import System.Process.Typed ()

main :: IO ()
main = do
  test
  print testing
