module Hix where

import Hix.Bootstrap (bootstrapProject)
import qualified Hix.Console as Console
import Hix.Console (errorMessage)
import qualified Hix.Data.Options as Options
import Hix.Data.Options (
  Command (..),
  GlobalOptions (GlobalOptions),
  LowerCommand (LowerInitCmd, LowerOptimizeCmd),
  ManagedCommand (ManagedCommitMsg),
  Options (Options),
  )
import Hix.Env (printEnvRunner)
import Hix.Error (
  Error (..),
  printBootstrapError,
  printEnvError,
  printError,
  printFatalError,
  printGhciError,
  printNewError,
  printPreprocError,
  )
import Hix.Ghci (printGhciCmdline, printGhcidCmdline)
import Hix.Managed.Bump.App (bumpCli)
import Hix.Managed.CommitMsg.App (managedCommitMsgCli)
import Hix.Managed.Lower.App (lowerInitCli, lowerOptimizeCli)
import Hix.Monad (M, runMWith)
import Hix.New (newProject)
import Hix.Options (parseCli)
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
  BootstrapError err -> printBootstrapError err
  NoMatch msg | fromMaybe False verbose -> printPreprocError msg
  NoMatch _ -> unit
  Fatal err -> printFatalError err
  Client err -> Console.err (errorMessage err)

runCommand :: Command -> M ()
runCommand = \case
  Preproc opts -> preprocess opts
  EnvRunner opts -> printEnvRunner opts.options
  GhcidCmd opts -> printGhcidCmdline opts
  GhciCmd opts -> printGhciCmdline opts
  NewCmd opts -> newProject opts.config
  BootstrapCmd opts -> bootstrapProject opts.config
  BumpCmd opts -> bumpCli opts
  LowerCmd sub -> case sub of
    LowerInitCmd opts -> lowerInitCli opts
    LowerOptimizeCmd opts -> lowerOptimizeCli opts
  ManagedCmd sub -> case sub of
    ManagedCommitMsg file -> managedCommitMsgCli file

main :: IO ()
main = do
  Options global cmd <- parseCli
  let verbose = fromMaybe False global.verbose
  leftA (printError verbose) =<< runMWith verbose global.debug global.quiet global.output global.cwd (runCommand cmd)
