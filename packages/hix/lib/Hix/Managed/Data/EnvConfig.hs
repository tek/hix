module Hix.Managed.Data.EnvConfig where

import Data.Aeson (FromJSON)

import Hix.Data.PackageName (LocalPackage)
import Hix.Data.VersionBounds (Bound)
import Hix.Managed.Cabal.Data.Config (GhcDb)
import Hix.Pretty (HPretty (hpretty), field, prettyFieldsV)

data EnvConfig =
  EnvConfig {
    targets :: [LocalPackage],
    ghc :: Maybe GhcDb,
    managedBound :: Maybe Bound
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty EnvConfig where
  hpretty EnvConfig {..} =
    prettyFieldsV [
      field "targets" targets,
      field "ghc" ghc,
      field "managedBound" managedBound
    ]
