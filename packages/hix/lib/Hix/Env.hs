module Hix.Env where

import qualified Data.Text.IO as Text

import Hix.Component (targetComponent)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (EnvRunner (EnvRunner), PackagesConfig, Target (Target))
import Hix.Data.Error (pathText)
import qualified Hix.Data.GhciConfig
import Hix.Json (jsonConfig)
import Hix.Monad (M)
import qualified Hix.Options as Options
import Hix.Options (EnvRunnerOptions, TargetSpec)

componentRunner :: PackagesConfig -> TargetSpec -> M (Maybe EnvRunner)
componentRunner config spec = do
  Target {component} <- targetComponent config spec
  pure component.runner

envRunner :: EnvRunnerOptions -> M EnvRunner
envRunner opts = do
  config <- either pure jsonConfig opts.config
  fromMaybe config.defaultEnv . join <$> traverse (componentRunner config.packages) opts.component

printEnvRunner :: EnvRunnerOptions -> M ()
printEnvRunner opts = do
  EnvRunner runner <- envRunner opts
  liftIO (Text.putStrLn (pathText runner))
