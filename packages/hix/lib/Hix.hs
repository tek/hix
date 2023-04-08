module Hix where

import Path.IO (getCurrentDir)

import Hix.Data.Error (Error (..), printEnvError, printFatalError, printGhciError, printNewError, printPreprocError)
import Hix.Env (printEnvRunner)
import Hix.Ghci (printGhciCmdline, printGhcidCmdline)
import Hix.Monad (M, runM)
import Hix.New (newProject)
import qualified Hix.Options as Options
import Hix.Options (
  Command (EnvRunner, GhciCmd, GhcidCmd, NewCmd, Preproc),
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
  NewError err -> printNewError err
  NoMatch msg | fromMaybe False verbose -> printPreprocError msg
  NoMatch _ -> unit
  Fatal err -> printFatalError err

runCommand :: Command -> M ()
runCommand = \case
  Preproc opts -> preprocess opts
  EnvRunner opts -> printEnvRunner opts.options
  GhcidCmd opts -> printGhcidCmdline opts
  GhciCmd opts -> printGhciCmdline opts
  NewCmd opts -> newProject opts.config

main :: IO ()
main = do
  Options global cmd <- parseCli
  cwd <- getCurrentDir
  leftA (handleError global) =<< runM cwd (runCommand cmd)
