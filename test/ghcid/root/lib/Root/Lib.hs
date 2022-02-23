module Root.Lib where

string :: String
string =
  case head [1, 2] of
    Just (1 :: Int) -> "Prelude works"
    _ -> "weird"
