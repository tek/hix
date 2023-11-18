module Hix.Managed.Data.ManagedConfig where

import Path (Abs, Dir, File, Path, Rel)

import Hix.Data.Bounds (TargetBound)
import Hix.Data.EnvName (EnvName)

data StateFileConfig =
  StateFileConfig {
    file :: Path Rel File,
    projectRoot :: Maybe (Path Abs Dir),
    updateProject :: Bool,
    latestOverrides :: Bool
  }
  deriving stock (Eq, Show, Generic)

data ManagedConfig =
  ManagedConfig {
    stateFile :: StateFileConfig,
    env :: EnvName,
    ghc :: Maybe (Path Abs Dir),
    targetBound :: TargetBound
  }
  deriving stock (Eq, Show, Generic)
