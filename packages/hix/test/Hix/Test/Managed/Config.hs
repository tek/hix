module Hix.Test.Managed.Config where

import Path (relfile)

import Hix.Managed.Data.ManagedConfig (StateFileConfig (..))
import Hix.Test.Utils (testRoot)

stateFileConfig :: StateFileConfig
stateFileConfig =
  StateFileConfig {
    file = [relfile|ops/managed.nix|],
    updateProject = True,
    projectRoot = Just testRoot
  }
