module Hix.Managed.Data.StateFileConfig where

import Path (Abs, Dir, File, Path, Rel, relfile)

data StateFileConfig =
  StateFileConfig {
    file :: Path Rel File,
    projectRoot :: Maybe (Path Abs Dir)
  }
  deriving stock (Eq, Show, Generic)

instance Default StateFileConfig where
  def =
    StateFileConfig {
      file = [relfile|ops/managed.nix|],
      projectRoot = Nothing
    }
