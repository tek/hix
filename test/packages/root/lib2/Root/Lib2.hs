module Root.Lib2 where

import Control.Monad.Trans.Reader ()
import Data.Aeson ()

import Root.Lib1 (lib1)

lib2 :: String
lib2 = "lib2/" ++ lib1
