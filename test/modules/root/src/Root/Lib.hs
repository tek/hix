module Root.Lib where

import qualified Sub.Lib
import qualified Dep1
import qualified Dep2

string :: String
string =
  Sub.Lib.string <> show Dep1.a <> show Dep2.a
