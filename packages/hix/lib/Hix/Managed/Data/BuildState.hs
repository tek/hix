module Hix.Managed.Data.BuildState where

import Distribution.Version (VersionRange)

import Hix.Data.ManagedEnv (ManagedState)
import qualified Hix.Data.Package
import Hix.Data.Package (Package (Package), PackageName)
import Hix.Data.Version (NewRange (NewRange))
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))

data BuildStatus =
  Success
  |
  Failure
  deriving stock (Eq, Show, Generic)

justSuccess :: a -> BuildStatus -> Maybe a
justSuccess a = \case
  Success -> Just a
  Failure -> Nothing

buildStatus :: Bool -> BuildStatus
buildStatus = \case
  True -> Success
  False -> Failure

data BuildSuccess =
  CandidateBuilt Candidate
  |
  RangeUpdated Package VersionRange
  |
  Unmodified PackageName
  deriving stock (Eq, Show, Generic)

buildSuccessPackage :: BuildSuccess -> PackageName
buildSuccessPackage = \case
  CandidateBuilt Candidate {package = Package {name}} -> name
  RangeUpdated Package {name} _ -> name
  Unmodified name -> name

modifiedCandidates :: [BuildSuccess] -> [Candidate]
modifiedCandidates =
  mapMaybe \case
    CandidateBuilt candidate -> Just candidate
    RangeUpdated package range -> Just Candidate {package, range = NewRange range}
    Unmodified _ -> Nothing

data BuildState a s =
  BuildState {
    success :: [BuildSuccess],
    failed :: [DepMutation a],
    state :: ManagedState,
    ext :: s
  }
  deriving stock (Eq, Show)

initBuildState :: ManagedState -> s -> BuildState a s
initBuildState state ext =
  BuildState {success = [], failed = [], state, ext}
