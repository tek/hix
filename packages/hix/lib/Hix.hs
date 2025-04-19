module Hix where

import System.Exit (exitFailure)

import Hix.Bootstrap (bootstrapProject)
import qualified Hix.Console as Console
import qualified Hix.Data.GlobalOptions
import Hix.Data.LogLevel (LogLevel)
import qualified Hix.Data.Options as Options
import Hix.Data.Options (Command (..), HackageCommand (..), InfoCommand (..), LowerCommand (..), Options (Options))
import Hix.Env (printEnvRunner, runEnvCommand)
import Hix.Error (Error, printError)
import Hix.Ghci (printGhciCmdline, printGhcidCmdline, runGhci, runGhcid)
import Hix.Managed.Bump.App (bumpCli)
import Hix.Managed.Lower.App (lowerAutoCli, lowerInitCli, lowerOptimizeCli, lowerStabilizeCli)
import Hix.Managed.ReleaseMaintenance (releaseMaintenanceCli, revisionCli)
import Hix.Monad (M, runMWith)
import Hix.New (initProject, newProject)
import Hix.Options (parseCli)
import Hix.Preproc (preprocess)

hixVersion :: Text
hixVersion = "0.9.1"

runCommand :: Command -> M ()
runCommand = \case
  Info InfoVersion -> Console.out hixVersion
  Preproc opts -> preprocess opts
  EnvRunner opts -> printEnvRunner opts.options
  GhcidCmd opts -> printGhcidCmdline opts
  GhciCmd opts -> printGhciCmdline opts
  RunGhci opts -> runGhci opts
  RunGhcid opts -> runGhcid opts
  RunCommand opts -> runEnvCommand opts
  Init opts -> initProject opts.config
  New opts -> newProject opts.config
  Bootstrap opts -> bootstrapProject opts.config
  Bump opts -> bumpCli opts
  Lower sub -> case sub of
    LowerInit opts -> lowerInitCli opts
    LowerOptimize opts -> lowerOptimizeCli opts
    LowerStabilize opts -> lowerStabilizeCli opts
    LowerAuto opts -> lowerAutoCli opts
  Hackage sub -> case sub of
    ReleaseMaint opts -> releaseMaintenanceCli opts
    Revision opts -> revisionCli opts

failure :: LogLevel -> Error -> IO ()
failure logLevel err = do
  printError logLevel err
  exitFailure

main :: IO ()
main = do
  Options global cmd <- parseCli
  leftA (failure global.logLevel) =<< runMWith global (runCommand cmd)
