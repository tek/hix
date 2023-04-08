module Hix.Data.GhciConfig where

import Data.Aeson (FromJSON, FromJSONKey)
import GHC.Exts (IsList)

import Hix.Data.ComponentConfig (EnvRunner, PackagesConfig)

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

data EnvConfig =
  EnvConfig {
    packages :: PackagesConfig,
    defaultEnv :: EnvRunner
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data GhciConfig =
  GhciConfig {
    packages :: PackagesConfig,
    setup :: Map RunnerName GhciSetupCode,
    run :: Map RunnerName GhciRunExpr,
    args :: GhciArgs
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
