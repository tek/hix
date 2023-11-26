module Hix.Managed.Data.Build where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.Extra (nubOrd, nubOrdOn)
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
import Hix.Managed.Data.ManagedConfig (ManagedOp)

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
    operation :: ManagedOp,
    modified :: [Candidate],
    unmodified :: [PackageName],
    failed :: [PackageName],
    modifiedNames :: Maybe Text,
    unmodifiedNames :: Maybe Text,
    failedNames :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

buildOutputFromLists :: ManagedOp -> [Candidate] -> [PackageName] -> [PackageName] -> BuildOutput
buildOutputFromLists operation modified' unmodified' failed' =
  BuildOutput {
    operation,
    modified,
    unmodified,
    failed,
    modifiedNames = comma ((.version.package) <$> modified),
    unmodifiedNames = comma unmodified,
    failedNames = comma failed
  }
  where
    modified = sortOn (.version.package) (nubOrdOn (.version.package) modified')
    unmodified = sort (nubOrd unmodified')
    failed = sort (nubOrd failed')
    comma = fmap (Text.intercalate ", " . fmap coerce . toList) . nonEmpty

emptyBuildOutput :: ManagedOp -> BuildOutput
emptyBuildOutput operation =
  BuildOutput {
    operation,
    modified = [],
    unmodified = [],
    failed = [],
    modifiedNames = Nothing,
    unmodifiedNames = Nothing,
    failedNames = Nothing
  }

combineBuildOutputs :: BuildOutput -> BuildOutput -> Either Text BuildOutput
combineBuildOutputs left right
  | left.operation /= right.operation
  = Left "Can't combine build outputs of different managed operations"
  | otherwise
  = Right (buildOutputFromLists right.operation modified unmodified failed)
  where
    modified = left.modified <> right.modified
    unmodified = left.unmodified <> right.unmodified
    failed = left.failed <> right.failed

buildOutput :: ManagedOp -> BuildResult a -> BuildOutput
buildOutput operation result =
  buildOutputFromLists operation modified unmodified failed
  where
    modified = sortOn (.version.package) (changed result)
    unmodified = sort (unchanged result)
    failed = sort ((.package) <$> result.failed)
