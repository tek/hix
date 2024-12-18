module Main where

import Data.Text (unpack)
import Root.Lib (text)

string :: String
string = unpack text

main :: IO ()
main =
  putStrLn string
