module Hix.Integration where

import qualified Hix.Data.GlobalOptions
import Hix.Data.Monad (M)
import Hix.Error (printError)
import Hix.Integration.Data.Options (Command (..), Options (..))
import Hix.Integration.Hackage (hackageServe)
import Hix.Integration.Options (parseCli)
import Hix.Integration.TuiTest (tuiTest)
import Hix.Monad.Run (runMWith)

runCommand :: Command -> M ()
runCommand = \case
  HackageServe opts -> hackageServe opts
  TuiTest -> tuiTest

main :: IO ()
main = do
  Options global cmd <- parseCli
  leftA (printError global.logLevel) =<< runMWith global (runCommand cmd)
