module Hix.Env where

import qualified Data.Text.IO as Text

import Hix.Component (targetComponent)
import Hix.Data.Error (pathText)
import qualified Hix.Data.GhciConfig as GhciConfig
import Hix.Data.GhciConfig (EnvRunner (EnvRunner), PackagesConfig, Target (Target))
import Hix.Monad (M)
import qualified Hix.Options as Options
import Hix.Options (EnvRunnerOptions (EnvRunnerOptions), TargetSpec)

componentRunner :: PackagesConfig -> TargetSpec -> M (Maybe EnvRunner)
componentRunner config spec = do
  Target {component} <- targetComponent config spec
  pure component.runner

envRunner :: EnvRunnerOptions -> M EnvRunner
envRunner EnvRunnerOptions {component, config} =
  fromMaybe config.defaultEnv . join <$> traverse (componentRunner config.packages) component

printEnvRunner :: EnvRunnerOptions -> M ()
printEnvRunner opts = do
  EnvRunner runner <- envRunner opts
  liftIO (Text.putStrLn (pathText runner))
