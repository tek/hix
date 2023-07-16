module Main where

import Root.Lib (string)
import Root.Lib2 (lib2)

main :: IO ()
main =
  putStrLn (string ++ "/" ++ lib2)
