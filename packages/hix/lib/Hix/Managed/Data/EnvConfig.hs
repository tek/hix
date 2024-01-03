module Hix.Managed.Data.EnvConfig where

import Data.Aeson (FromJSON)

import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Cabal.Data.Config (GhcDb)

data EnvConfig =
  EnvConfig {
    targets :: [LocalPackage],
    ghc :: GhcDb
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
