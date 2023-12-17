module Hix.Managed.Data.ManagedOp where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Exon (exon)

-- TODO replace this with proper abstraction
data ManagedOp =
  OpBump
  |
  OpLowerInit
  |
  OpLowerOptimize
  |
  OpLowerStabilize
  deriving stock (Eq, Show, Generic)

instance ToJSON ManagedOp where
  toJSON =
    toJSON @Text . \case
      OpBump -> "bump"
      OpLowerInit -> "lower-init"
      OpLowerOptimize -> "lower-optimize"
      OpLowerStabilize -> "lower-stabilize"

instance FromJSON ManagedOp where
  parseJSON =
    parseJSON @Text >=> \case
      "bump" -> pure OpBump
      "lower-init" -> pure OpLowerInit
      "lower-optimize" -> pure OpLowerOptimize
      "lower-stabilize" -> pure OpLowerStabilize
      v -> fail [exon|Invalid value for ManagedOp: #{toString v}|]
