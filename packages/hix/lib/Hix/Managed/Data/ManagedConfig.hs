module Hix.Managed.Data.ManagedConfig where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Exon (exon)
import Path (Abs, Dir, File, Path, Rel)

import Hix.Data.Bounds (TargetBound)
import Hix.Data.EnvName (EnvName)

data StateFileConfig =
  StateFileConfig {
    file :: Path Rel File,
    projectRoot :: Maybe (Path Abs Dir),
    updateProject :: Bool
  }
  deriving stock (Eq, Show, Generic)

data ManagedOp =
  OpBump
  |
  OpLower
  deriving stock (Eq, Show, Generic)

instance ToJSON ManagedOp where
  toJSON =
    toJSON @Text . \case
      OpBump -> "bump"
      OpLower -> "lower"

instance FromJSON ManagedOp where
  parseJSON =
    parseJSON @Text >=> \case
      "bump" -> pure OpBump
      "lower" -> pure OpLower
      v -> fail [exon|Invalid value for ManagedOp: #{toString v}|]

data ManagedConfig =
  ManagedConfig {
    stateFile :: StateFileConfig,
    operation :: ManagedOp,
    envs :: [EnvName],
    ghc :: Maybe (Path Abs Dir),
    targetBound :: TargetBound
  }
  deriving stock (Eq, Show, Generic)
