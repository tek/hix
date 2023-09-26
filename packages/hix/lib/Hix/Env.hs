module Hix.Env where

import qualified Data.Text.IO as Text
import Path (Abs, Dir, Path)

import Hix.Component (targetComponent)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (
  EnvRunner (EnvRunner),
  PackageName,
  PackagesConfig,
  TargetOrDefault (DefaultTarget, ExplicitTarget),
  )
import Hix.Data.Error (pathText)
import qualified Hix.Data.GhciConfig
import Hix.Json (jsonConfig)
import Hix.Monad (M)
import qualified Hix.Options as Options
import Hix.Options (EnvRunnerOptions, TargetSpec)

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
  config <- either pure jsonConfig opts.config
  let runner = componentRunner opts.root config.mainPackage config.packages
  fromMaybe config.defaultEnv . join <$> traverse runner opts.component

printEnvRunner :: EnvRunnerOptions -> M ()
printEnvRunner opts = do
  EnvRunner runner <- envRunner opts
  liftIO (Text.putStrLn (pathText runner))
