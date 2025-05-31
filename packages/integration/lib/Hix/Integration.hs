module Hix.Integration where

import qualified Hix.Data.GlobalOptions
import Hix.Error (printError)
import Hix.Integration.Data.Options (Command (..), Options (..))
import Hix.Integration.Hackage (hackageServe)
import Hix.Integration.Options (parseCli)
import Hix.Monad (M, runMWith)

runCommand :: Command -> M ()
runCommand = \case
  HackageServe opts -> hackageServe opts

main :: IO ()
main = do
  Options global cmd <- parseCli
  leftA (printError global.logLevel) =<< runMWith global (runCommand cmd)
