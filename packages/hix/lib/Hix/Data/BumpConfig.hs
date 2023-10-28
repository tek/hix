module Hix.Data.BumpConfig where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Distribution.Types.Version (Version)
import Distribution.Version (VersionRange)
import Path (Abs, Dir, File, Path, Rel)

import Hix.Data.ComponentConfig (EnvName, PackageName, PackagesDeps, jsonParsec)
import Hix.Data.Version (SourceHash)

newtype ManagedDeps =
  ManagedDeps (Map PackageName VersionRange)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance FromJSON ManagedDeps where
  parseJSON v = do
    raw <- parseJSON v
    versions <- traverse jsonParsec raw
    pure (ManagedDeps versions)

newtype PackagesManaged =
  PackagesManaged (Map PackageName ManagedDeps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid)

newtype OverrideVersion =
  OverrideVersion Version
  deriving stock (Eq, Show, Generic)

instance FromJSON OverrideVersion where
  parseJSON v = do
    raw <- parseJSON v
    OverrideVersion <$> jsonParsec raw

data OverrideManaged =
  OverrideManaged {
    version :: OverrideVersion,
    hash :: SourceHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

newtype OverridesEnvManaged =
  OverridesEnvManaged (Map PackageName OverrideManaged)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid)

newtype OverridesManaged =
  OverridesManaged (Map EnvName OverridesEnvManaged)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid)

data ManagedConfig =
  ManagedConfig {
    deps :: PackagesManaged,
    overrides :: OverridesManaged
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ManagedConfig where
  parseJSON = withObject "ManagedConfig" \ o -> do
    deps <- o .: "deps" <|> pure mempty
    overrides <- o .: "overrides" <|> pure mempty
    pure (ManagedConfig deps overrides)

data BumpEnv =
  BumpEnv {
    deps :: PackagesDeps,
    managed :: ManagedConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data BumpConfig =
  BumpConfig {
    env :: EnvName,
    package :: Maybe PackageName,
    file :: Path Rel File,
    updateProject :: Bool,
    latestOverrides :: Bool,
    projectRoot :: Maybe (Path Abs Dir)
  }
  deriving stock (Eq, Show, Generic)
