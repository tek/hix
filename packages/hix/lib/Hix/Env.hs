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
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.GhciConfig
import Hix.Data.GhciConfig (CommandEnvContext (..), CommandContext)
import Hix.Data.Monad (LogLevel (..), M)
import Hix.Data.Options (CommandOptions (..), RunCommandOptions (..), TargetSpec)
import Hix.Data.PackageName (PackageName)
import Hix.Error (pathText, printError)
import Hix.Json (jsonConfigE)
import qualified Hix.Log as Log
import qualified Hix.Managed.Handlers.Context as Context
import Hix.Managed.Handlers.Context (ContextHandlers, ContextKey (ContextCommandEnv), queryContext)
import Hix.Maybe (fromMaybeA)
import Hix.Monad (appContext, catchM, fatalError)
import Hix.Path (resolvePathSpec)

componentRunner ::
  Maybe (Path Abs Dir) ->
  Maybe PackageName ->
  PackagesConfig ->
  TargetSpec ->
  M (Maybe EnvName)
componentRunner cliRoot defaultPkg config spec =
  targetComponent cliRoot defaultPkg config (Just spec) <&> \case
    ExplicitTarget t -> t.component.env
    DefaultTarget t -> t.component.env
    _ -> Nothing

commandEnv ::
  ContextHandlers ->
  CommandContext ->
  CommandOptions ->
  M CommandEnvContext
commandEnv contextHandlers context options =
  appContext "resolving the command env" do
    root <- traverse resolvePathSpec options.root
    let fromComponent = componentRunner root context.mainPackage context.packages
        resolve = fromMaybe (fromMaybe "dev" context.defaultEnv) . join <$> traverse fromComponent options.component
    name <- fromMaybeA resolve options.env
    catchM (queryContext contextHandlers (ContextCommandEnv name)) \ orig -> do
      printError LogVerbose orig
      fatalError (noEnv name)
  where
    noEnv name = [exon|No such environment: #{Color.env name}|]

printEnvRunner ::
  RunCommandOptions ->
  M ()
printEnvRunner options = do
  context <- jsonConfigE options.context
  CommandEnvContext {runner = EnvRunner runner} <- commandEnv Context.handlersProd context options.command
  liftIO (Text.putStrLn (pathText runner))

runEnvProcess ::
  CommandEnvContext ->
  Text ->
  [Text] ->
  M ()
runEnvProcess CommandEnvContext {runner = EnvRunner runner} exe args = do
  let cmd = Text.unwords (exe : args)
  Log.debug [exon|Starting process: #{pathText runner} #{cmd}|]
  runProcess (setStderr inherit (setStdout inherit (proc (toFilePath runner) (toString <$> exe : args)))) >>= \case
    ExitSuccess -> unit
    ExitFailure n -> fatalError [exon|#{Color.shellCommand exe} exited with code #{Color.number n}|]

runEnvCommand ::
  RunCommandOptions ->
  M ()
runEnvCommand options@RunCommandOptions {exe, args} = do
  context <- jsonConfigE options.context
  config <- commandEnv Context.handlersProd context options.command
  runEnvProcess config exe args
