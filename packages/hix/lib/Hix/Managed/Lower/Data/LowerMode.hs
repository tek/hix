module Hix.Managed.Lower.Data.LowerMode where

import Hix.Data.Bounds (TargetBound (TargetLower))
import Hix.Managed.Build.Mutation (MutationResult (MutationFailed, MutationKeep))
import Hix.Managed.Data.ManagedOp (ManagedOp (OpLowerInit, OpLowerOptimize, OpLowerStabilize))
import Hix.Managed.Lower.Data.Lower (LowerState)

-- TODO move this out of ManagedApp so it can be set from @lowerInit@ etc. to avoid duplication in tests
data LowerMode =
  LowerMode {
    operation :: ManagedOp,
    targetBound :: TargetBound,
    firstSuccess :: Bool,
    noSuccess :: MutationResult LowerState
  }
  deriving stock (Eq, Show, Generic)

lowerInitMode :: LowerMode
lowerInitMode =
  LowerMode {
    operation = OpLowerInit,
    targetBound = TargetLower,
    firstSuccess = True,
    noSuccess = MutationFailed
  }

lowerOptimizeMode :: LowerMode
lowerOptimizeMode =
  LowerMode {
    operation = OpLowerOptimize,
    targetBound = TargetLower,
    firstSuccess = False,
    noSuccess = MutationKeep
  }

lowerStabilizeMode :: LowerMode
lowerStabilizeMode =
  LowerMode {
    operation = OpLowerStabilize,
    targetBound = TargetLower,
    firstSuccess = True,
    noSuccess = MutationFailed
  }
