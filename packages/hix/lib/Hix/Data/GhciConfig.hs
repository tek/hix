module Hix.Data.GhciConfig where

import Data.Aeson (FromJSON (parseJSON), FromJSONKey)
import GHC.Exts (IsList)
import Path (Abs, Dir, File, Path, Rel)

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

newtype PackageName =
  PackageName { unPackageName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey)

newtype ComponentName =
  ComponentName { unComponentName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey)

data ComponentConfig =
  ComponentConfig {
    name :: ComponentName,
    sourceDirs :: SourceDirs,
    runner :: Path Abs File
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

newtype RunnerName =
  RunnerName { unRunnerName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSONKey)

newtype GhciSetupCode =
  GhciSetupCode { unGhciSetupCode :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, Semigroup, Monoid)

newtype GhciRunExpr =
  GhciRunExpr { unGhciRunExpr :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON)

newtype GhciArgs =
  GhciArgs { unGhciArgs :: [Text] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsList, Ord, FromJSON)

data GhciConfig =
  GhciConfig {
    packages :: Map PackageName PackageConfig,
    setup :: Map RunnerName GhciSetupCode,
    run :: Map RunnerName GhciRunExpr,
    args :: GhciArgs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
