module Hix.Managed.Data.MaintContext where

import Data.Aeson (FromJSON (..), defaultOptions, genericParseJSON)
import Path (Dir, Path, Rel)
import Text.PrettyPrint (($+$))

import Hix.Data.EnvName (EnvName)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.VersionBounds (Bound)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Pretty (HPretty (hpretty), field, prettyFieldsV, prettyMap)

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

-- | @since unreleased
data MaintEnv =
  MaintEnv {
    targets :: [LocalPackage],
    managedBound :: Maybe Bound
  }
  deriving stock (Eq, Show, Generic)

instance HPretty MaintEnv where
  hpretty MaintEnv {..} =
    prettyFieldsV [
      field "targets" targets,
      field "managedBound" managedBound
    ]

-- | The type of the field 'envs' was changed from @[LocalPackage]@ to 'MaintEnv', so this instance has to try both
-- protocols.
instance FromJSON MaintEnv where
  parseJSON v =
    compat <|> genericParseJSON defaultOptions v
    where
      compat = do
        targets <- parseJSON v
        pure MaintEnv {targets, managedBound = Nothing}

-- | @since unreleased
data MaintContextProto =
  MaintContextProto {
    packages :: Packages MaintPackage,
    hackage :: Map HackageName ContextHackageRepo,
    envs :: Envs MaintEnv
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty MaintContextProto where
  hpretty MaintContextProto {..} =
    prettyMap "maint" [
      field "packages" packages,
      field "hackage" hackage,
      field "envs" envs
    ]

data MaintContext =
  MaintContext {
    packages :: Packages MaintPackage,
    hackage :: Map HackageName ContextHackageRepo,
    targetEnvs :: Packages EnvName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty MaintContext where
  hpretty MaintContext {..} =
    prettyMap "maint" [
      field "packages" packages,
      field "hackage" hackage,
      field "targetEnvs" targetEnvs
    ]
