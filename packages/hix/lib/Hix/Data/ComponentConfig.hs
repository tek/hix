module Hix.Data.ComponentConfig where

import Data.Aeson (FromJSON (parseJSON), FromJSONKey, withObject, (.:))
import Path (Abs, Dir, File, Path, Rel)

import Hix.Data.Package (PackageName)

newtype PackagePath =
  PackagePath { unPackagePath :: Path Rel Dir }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (FromJSON, FromJSONKey)

newtype SourceDir =
  SourceDir { unSourceDir :: Path Rel Dir }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON)

newtype SourceDirs =
  SourceDirs { unSourceDirs :: [SourceDir] }
  deriving stock (Eq, Show, Generic)

instance FromJSON SourceDirs where
  parseJSON v =
    (SourceDirs <$> parseJSON v)
    <|>
    (SourceDirs . pure <$> parseJSON v)

newtype ModuleName =
  ModuleName { unModuleName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey)

newtype ComponentName =
  ComponentName { unComponentName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey)

newtype EnvRunner =
  EnvRunner (Path Abs File)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON)

data PreludePackage =
  PreludePackageName Text
  |
  PreludePackageSpec { name :: Text }
  deriving stock (Eq, Show, Generic)

instance FromJSON PreludePackage where
  parseJSON v =
    hpackStruct v <|> plainName
    where
      hpackStruct = withObject "PreludePackageSpec" \ o -> o .: "name"
      plainName = PreludePackageName <$> parseJSON v

data PreludeConfig =
  PreludeConfig {
    package :: PreludePackage,
    module_ :: ModuleName
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON PreludeConfig where
  parseJSON =
    withObject "PreludeConfig" \ o -> do
      package <- o .: "package"
      module_ <- o .: "module"
      pure PreludeConfig {..}

data ComponentConfig =
  ComponentConfig {
    name :: ComponentName,
    sourceDirs :: SourceDirs,
    runner :: Maybe EnvRunner,
    extensions :: [String],
    language :: String,
    ghcOptions :: [String],
    prelude :: Maybe PreludeConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data PackageConfig =
  PackageConfig {
    name :: PackageName,
    src :: Path Rel Dir,
    components :: Map ComponentName ComponentConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

type PackagesConfig = Map PackageName PackageConfig

data Target =
  Target {
    package :: PackageConfig,
    component :: ComponentConfig,
    sourceDir :: Maybe SourceDir
  }
  deriving stock (Eq, Show, Generic)

data TargetOrDefault =
  ExplicitTarget Target
  |
  DefaultTarget Target
  |
  NoDefaultTarget Text
  deriving stock (Eq, Show, Generic)
