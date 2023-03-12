module Root.Lib where

import Dep.Lib

string :: String
string =
  case head [1, 2] of
    Just (1 :: Int) -> preludeName <> " works"
    _ -> "weird"
