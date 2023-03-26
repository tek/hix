module Hix where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Hix.Data.Error (Error (..), printGhcidError, printPreprocError)
import Hix.Ghci (printGhciCmdline, printGhciEnv, printGhcidCmdline)
import Hix.Options (Command (GhciCmd, GhciEnv, GhcidCmd, Preproc), GlobalOptions (..), Options (Options), parseCli)
import Hix.Preproc (preprocess)

handleError ::
  MonadIO m =>
  GlobalOptions ->
  Error ->
  m ()
handleError GlobalOptions {verbose} = \case
  PreprocError err -> printPreprocError err
  GhcidError err -> printGhcidError err
  NoMatch msg | (fromMaybe False verbose) -> printPreprocError msg
  NoMatch _ -> unit

runCommand :: Command -> ExceptT Error IO ()
runCommand = \case
  Preproc opts -> preprocess opts
  GhciEnv opts -> printGhciEnv opts
  GhcidCmd opts -> printGhcidCmdline opts
  GhciCmd opts -> printGhciCmdline opts

main :: IO ()
main = do
  Options global cmd <- parseCli
  leftA (handleError global) =<< runExceptT (runCommand cmd)
