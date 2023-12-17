module Hix.Managed.Data.ManagedConfig where

import Path (Abs, Dir, File, Path, Rel)

import Hix.Data.EnvName (EnvName)

data StateFileConfig =
  StateFileConfig {
    file :: Path Rel File,
    projectRoot :: Maybe (Path Abs Dir),
    updateProject :: Bool
  }
  deriving stock (Eq, Show, Generic)

data ManagedConfig =
  ManagedConfig {
    stateFile :: StateFileConfig,
    envs :: [EnvName]
  }
  deriving stock (Eq, Show, Generic)
