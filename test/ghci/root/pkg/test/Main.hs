module Main where

import Root.Lib (test)
import Testing (testing)

main :: IO ()
main = do
  test
  print testing
