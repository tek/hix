module Main where

import Polysemy
import Root.Lib (string)

main :: IO ()
main =
  putStrLn string
