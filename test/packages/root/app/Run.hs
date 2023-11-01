module Main where

import Data.Hashable

import Root.Lib (string)
import Root.Lib2 (lib2)

main :: IO ()
main =
  putStrLn (string ++ "/" ++ lib2)
