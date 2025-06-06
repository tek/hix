module Hix.Managed.Lower.Data.LowerMode where

import Hix.Managed.Data.Mutation (MutationResult (MutationFailed, MutationKeep))

data LowerMode =
  LowerMode {
    firstSuccess :: Bool,
    noSuccess :: MutationResult
  }
  deriving stock (Eq, Show, Generic)

lowerInitMode :: LowerMode
lowerInitMode =
  LowerMode {
    firstSuccess = True,
    noSuccess = MutationFailed
  }

lowerOptimizeMode :: LowerMode
lowerOptimizeMode =
  LowerMode {
    firstSuccess = False,
    noSuccess = MutationKeep
  }

lowerStabilizeMode :: LowerMode
lowerStabilizeMode =
  LowerMode {
    firstSuccess = True,
    noSuccess = MutationFailed
  }
