module Hix.Data.LowerConfig where

import qualified Hix.Data.Options
import Hix.Data.Options (LowerOptions (LowerOptions))
import Hix.Managed.Build.Mutation (MutationResult (MutationFailed, MutationKeep))
import Hix.Managed.Data.SolverParams (SolverParams)
import Hix.Managed.Lower.Data.Lower (LowerState)

data LowerConfig =
  LowerConfig {
    firstSuccess :: Bool,
    noSuccess :: MutationResult LowerState,
    maxFailedPre :: Int,
    maxFailedPost :: Int,
    oldest :: Bool,
    initialSolverParams :: SolverParams
  }
  deriving stock (Eq, Show, Generic)

lowerConfig :: LowerOptions -> Bool -> MutationResult LowerState -> LowerConfig
lowerConfig LowerOptions {..} firstSuccess noSuccess =
  LowerConfig {oldest = False, initialSolverParams = [], ..}

defaultLowerConfig :: Bool -> MutationResult LowerState -> LowerConfig
defaultLowerConfig firstSuccess noSuccess =
  LowerConfig {
    firstSuccess,
    noSuccess,
    maxFailedPre = 99,
    maxFailedPost = 0,
    oldest = False,
    initialSolverParams = mempty
  }

lowerConfigInit :: LowerConfig
lowerConfigInit = defaultLowerConfig True MutationFailed

lowerConfigOptimize :: LowerConfig
lowerConfigOptimize = defaultLowerConfig False MutationKeep

lowerConfigStabilize :: LowerConfig
lowerConfigStabilize = defaultLowerConfig True MutationFailed

data LowerInitConfig =
  LowerInitConfig {
    reset :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default LowerInitConfig where
  def = LowerInitConfig {reset = False}
