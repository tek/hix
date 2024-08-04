module Hix.Managed.Data.MaintContext where

import Data.Aeson (FromJSON)
import Path (Dir, Path, Rel)
import Text.PrettyPrint (($+$))

import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Pretty (HPretty (hpretty), prettyMap, field)

data MaintPackage =
  MaintPackage {
    package :: ManagedPackage,
    path :: Path Rel Dir
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty MaintPackage where
  hpretty MaintPackage {..} =
    hpretty package $+$ hpretty path

data MaintContext =
  MaintContext {
    packages :: Packages MaintPackage,
    hackage :: Map HackageName ContextHackageRepo,
    envs :: Envs [LocalPackage]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty MaintContext where
  hpretty MaintContext {..} =
    prettyMap "maint" [
      field "packages" packages,
      field "hackage" hackage,
      field "envs" envs
    ]
