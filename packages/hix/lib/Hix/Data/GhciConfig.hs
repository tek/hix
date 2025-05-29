module Hix.Data.GhciConfig where

import Data.Aeson (FromJSON, FromJSONKey)
import GHC.Exts (IsList)

import Hix.Data.ComponentConfig (EnvRunner, PackagesConfig)
import Hix.Data.EnvName (EnvName)
import Hix.Data.PackageName (PackageName)
import Hix.Pretty (HPretty (..), prettyFieldsV, field)

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
  GhciArgs { text :: [Text] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsList, Ord, FromJSON, Semigroup, Monoid, HPretty)

newtype GhcidArgs =
  GhcidArgs { text :: [Text] }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsList, Ord, FromJSON, Semigroup, Monoid, HPretty)

newtype ChangeDir =
  ChangeDir { unChangeDir :: Bool }
  deriving stock (Eq, Show, Generic)

data CommandContext =
  CommandContext {
    packages :: PackagesConfig,
    mainPackage :: Maybe PackageName,
    defaultEnv :: Maybe EnvName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty CommandContext where
  hpretty CommandContext {..} =
    prettyFieldsV [
      field "packages" packages,
      field "mainPackage" mainPackage,
      field "defaultEnv" defaultEnv
    ]

data CommandEnvContext =
  CommandEnvContext {
    runner :: EnvRunner,
    ghciArgs :: GhciArgs,
    ghcidArgs :: GhcidArgs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance HPretty CommandEnvContext where
  hpretty CommandEnvContext {..} =
    prettyFieldsV [
      field "runner" runner,
      field "ghciArgs" ghciArgs,
      field "ghcidArgs" ghcidArgs
    ]

data CommandConfig =
  CommandConfig {
    defaultEnv :: EnvName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data GhciContext =
  GhciContext {
    command :: CommandContext,
    setup :: Map RunnerName GhciSetupCode,
    run :: Map RunnerName GhciRunExpr,
    args :: GhciArgs,
    manualCabal :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data GhciConfig =
  GhciConfig {
    command :: CommandConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
