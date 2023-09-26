module Hix.Data.GhciConfig where

import Data.Aeson (FromJSON, FromJSONKey)
import GHC.Exts (IsList)

import Hix.Data.ComponentConfig (EnvRunner, PackageName, PackagesConfig)

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

newtype ChangeDir =
  ChangeDir { unChangeDir :: Bool }
  deriving stock (Eq, Show, Generic)

data EnvConfig =
  EnvConfig {
    packages :: PackagesConfig,
    defaultEnv :: EnvRunner,
    mainPackage :: Maybe PackageName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data GhciConfig =
  GhciConfig {
    packages :: PackagesConfig,
    mainPackage :: Maybe PackageName,
    setup :: Map RunnerName GhciSetupCode,
    run :: Map RunnerName GhciRunExpr,
    args :: GhciArgs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
