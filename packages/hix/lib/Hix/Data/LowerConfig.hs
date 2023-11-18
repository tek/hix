module Hix.Data.LowerConfig where

import Hix.Managed.Data.SolverBounds (SolverBounds)

-- TODO unify these
data LowerInitConfig =
  LowerInitConfig {
    stabilize :: Bool,
    lowerMajor :: Bool,
    oldest :: Bool,
    initialBounds :: SolverBounds
  }
  deriving stock (Eq, Show, Generic)

data LowerOptimizeConfig =
  LowerOptimizeConfig {
    oldest :: Bool,
    initialBounds :: SolverBounds
  }
  deriving stock (Eq, Show, Generic)
