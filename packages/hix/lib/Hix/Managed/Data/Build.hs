module Hix.Managed.Data.Build where

import Data.Aeson (ToJSON)
import qualified Data.Text as Text
import Distribution.Version (VersionRange)

import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Package (PackageName (PackageName))
import qualified Hix.Data.Version
import Hix.Data.Version (NewRange (NewRange), NewVersion)
import qualified Hix.Managed.Build.Mutation
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

unchanged :: BuildResult a -> [PackageName]
unchanged BuildResult {success} =
  flip mapMaybe success \case
    CandidateBuilt _ -> Nothing
    RangeUpdated _ _ -> Nothing
    Unmodified package -> Just package

data BuildOutput =
  BuildOutput {
    modified :: [Candidate],
    unmodified :: [PackageName],
    failed :: [PackageName],
    modifiedNames :: Maybe Text,
    unmodifiedNames :: Maybe Text,
    failedNames :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

buildOutput :: BuildResult a -> BuildOutput
buildOutput result =
  BuildOutput {
    modified,
    unmodified,
    failed,
    modifiedNames = comma ((.version.package) <$> modified),
    unmodifiedNames = comma unmodified,
    failedNames = comma failed
  }
  where
    modified = sortOn (.version.package) (changed result)
    unmodified = sort (unchanged result)
    failed = sort ((.package) <$> result.failed)
    comma = fmap (Text.intercalate ", " . fmap coerce . toList) . nonEmpty
