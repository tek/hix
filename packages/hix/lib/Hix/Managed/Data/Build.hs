module Hix.Managed.Data.Build where

import Distribution.Version (VersionRange)

import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Package (PackageName)
import Hix.Data.Version (NewRange (NewRange), NewVersion)
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))

data BuildSuccess =
  CandidateBuilt Candidate
  |
  RangeUpdated NewVersion VersionRange
  |
  Unmodified PackageName
  deriving stock (Eq, Show, Generic)

data BuildResult a =
  BuildResult {
    success :: [BuildSuccess],
    failed :: [DepMutation a],
    managed :: ManagedState
  }
  deriving stock (Eq, Show)

data BuildState a s =
  BuildState {
    success :: [BuildSuccess],
    failed :: [DepMutation a],
    managed :: ManagedState,
    ext :: s
  }
  deriving stock (Eq, Show)

buildResult :: BuildState a s -> BuildResult a
buildResult BuildState {..} = BuildResult {..}

changed :: BuildResult a -> [Candidate]
changed BuildResult {success} =
  flip mapMaybe success \case
    CandidateBuilt candidate -> Just candidate
    RangeUpdated version range -> Just Candidate {version, range = NewRange range}
    Unmodified _ -> Nothing
