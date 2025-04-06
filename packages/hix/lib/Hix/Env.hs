module Hix.Env where

import qualified Data.Text.IO as Text
import Path (Abs, Dir, Path)

import Hix.Component (targetComponent)
import qualified Hix.Data.ComponentConfig
import Hix.Data.ComponentConfig (EnvRunner (EnvRunner), PackagesConfig, TargetOrDefault (DefaultTarget, ExplicitTarget))
import qualified Hix.Data.GhciConfig
import Hix.Data.Monad (M)
import qualified Hix.Data.Options as Options
import Hix.Data.Options (EnvRunnerOptions, TargetSpec)
import Hix.Data.PackageName (PackageName)
import Hix.Error (pathText)
import Hix.Json (jsonConfigE)
import Hix.Monad (resolvePathSpecDir)

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
  root <- mapM resolvePathSpecDir opts.root
  let runner = componentRunner root config.mainPackage config.packages
  fromMaybe config.defaultEnv . join <$> traverse runner opts.component

printEnvRunner :: EnvRunnerOptions -> M ()
printEnvRunner opts = do
  EnvRunner runner <- envRunner opts
  liftIO (Text.putStrLn (pathText runner))
