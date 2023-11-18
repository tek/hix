module Hix.Managed.Data.Build where

import Hix.Data.ManagedEnv (ManagedState)
import Hix.Managed.Build.Mutation (Candidate, DepMutation)

data BuildResult a =
  BuildResult {
    success :: [Candidate],
    failed :: [DepMutation a],
    managed :: ManagedState
  }
  deriving stock (Eq, Show)

data BuildState a s =
  BuildState {
    success :: [Candidate],
    failed :: [DepMutation a],
    managed :: ManagedState,
    ext :: s
  }
  deriving stock (Eq, Show)

buildResult :: BuildState a s -> BuildResult a
buildResult BuildState {..} = BuildResult {..}
