module Hix where

import Control.Monad.Trans.Class (lift)
import Path.IO (getCurrentDir)

import Hix.Data.Error (Error (..), printEnvError, printGhciError, printPreprocError)
import Hix.Env (printEnvRunner)
import Hix.Ghci (printGhciCmdline, printGhcidCmdline)
import Hix.Monad (M, runM)
import qualified Hix.Options as Options
import Hix.Options (
  Command (EnvRunner, GhciCmd, GhcidCmd, Preproc),
  GlobalOptions (GlobalOptions),
  Options (Options),
  parseCli,
  )
import Hix.Preproc (preprocess)

handleError ::
  MonadIO m =>
  GlobalOptions ->
  Error ->
  m ()
handleError GlobalOptions {verbose} = \case
  PreprocError err -> printPreprocError err
  EnvError err -> printEnvError err
  GhciError err -> printGhciError err
  NoMatch msg | fromMaybe False verbose -> printPreprocError msg
  NoMatch _ -> unit

runCommand :: Command -> M ()
runCommand = \case
  Preproc opts -> lift (preprocess opts)
  EnvRunner opts -> printEnvRunner opts.options
  GhcidCmd opts -> printGhcidCmdline opts
  GhciCmd opts -> printGhciCmdline opts

main :: IO ()
main = do
  Options global cmd <- parseCli
  cwd <- getCurrentDir
  leftA (handleError global) =<< runM cwd (runCommand cmd)
