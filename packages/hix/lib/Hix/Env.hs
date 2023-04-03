module Hix.Env where

import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Text.IO as Text

import Hix.Component (targetComponent)
import Hix.Data.Error (Error, pathText)
import qualified Hix.Data.GhciConfig as GhciConfig
import Hix.Data.GhciConfig (EnvRunner (EnvRunner), PackagesConfig)
import qualified Hix.Options as Options
import Hix.Options (ComponentSpec, EnvRunnerOptions (EnvRunnerOptions))

componentRunner :: PackagesConfig -> ComponentSpec -> ExceptT Error IO (Maybe EnvRunner)
componentRunner config component = do
    (_, target) <- targetComponent config component
    pure target.runner

envRunner :: EnvRunnerOptions -> ExceptT Error IO EnvRunner
envRunner EnvRunnerOptions{component, config} =
  fromMaybe config.defaultEnv . join <$> traverse (componentRunner config.packages) component

printEnvRunner :: EnvRunnerOptions -> ExceptT Error IO ()
printEnvRunner opts = do
  EnvRunner runner <- envRunner opts
  liftIO (Text.putStrLn (pathText runner))
