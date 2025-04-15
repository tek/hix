module Hix.Env where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, Dir, Path, toFilePath)
import System.Exit (ExitCode (..))
import System.Process.Typed (inherit, proc, runProcess, setStderr, setStdout)

import qualified Hix.Color as Color
import Hix.Component (targetComponent)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (EnvRunner (..), PackagesConfig, TargetOrDefault (..))
import qualified Hix.Data.GhciConfig
import Hix.Data.Monad (M)
import qualified Hix.Data.Options as Options
import Hix.Data.Options (CommandOptions (..), EnvRunnerOptions, TargetSpec)
import Hix.Data.PackageName (PackageName)
import Hix.Error (pathText)
import Hix.Json (jsonConfigE)
import qualified Hix.Log as Log
import Hix.Monad (fatalError)
import Hix.Path (resolvePathSpec)

-- TODO when there is a solution for default command env fallback configuration, the DefaultTarget case must return
-- Nothing when the config requests it
componentRunner ::
  Maybe (Path Abs Dir) ->
  Maybe PackageName ->
  PackagesConfig ->
  TargetSpec ->
  M (Maybe EnvRunner)
componentRunner cliRoot defaultPkg config spec =
  targetComponent cliRoot defaultPkg config spec <&> \case
    ExplicitTarget t -> t.component.runner
    DefaultTarget t -> t.component.runner
    _ -> Nothing

envRunner :: EnvRunnerOptions -> M EnvRunner
envRunner opts = do
  config <- jsonConfigE opts.config
  root <- traverse resolvePathSpec opts.root
  let runner = componentRunner root config.mainPackage config.packages
  fromMaybe config.defaultEnv . join <$> traverse runner opts.component

printEnvRunner :: EnvRunnerOptions -> M ()
printEnvRunner opts = do
  EnvRunner runner <- envRunner opts
  liftIO (Text.putStrLn (pathText runner))

runEnvProcess ::
  EnvRunnerOptions ->
  Text ->
  [Text] ->
  M ()
runEnvProcess options exe args = do
  EnvRunner runner <- envRunner options
  let cmd = Text.unwords (exe : args)
  Log.debug [exon|Starting process: #{pathText runner} #{cmd}|]
  runProcess (setStderr inherit (setStdout inherit (proc (toFilePath runner) (toString <$> exe : args)))) >>= \case
    ExitSuccess -> unit
    ExitFailure n -> fatalError [exon|#{Color.shellCommand exe} exited with code #{Color.number n}|]

runEnvCommand :: CommandOptions -> M ()
runEnvCommand CommandOptions {..} = runEnvProcess env exe args
