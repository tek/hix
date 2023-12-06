module Hix.Managed.Data.Build where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.Extra (nubOrd, nubOrdOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Distribution.Version (VersionRange)

import qualified Hix.Data.Bounds
import Hix.Data.Bounds (RemovableBounds (RemovableBounds))
import Hix.Data.Deps (TargetDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.ManagedEnv (ManagedEnvState, ManagedState)
import Hix.Data.Package (PackageName (PackageName))
import qualified Hix.Data.Version
import Hix.Data.Version (NewRange (NewRange), NewVersion)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import Hix.Managed.Data.ManagedConfig (ManagedOp)
import Hix.Managed.State (envStateWithMutations)

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
  RangeUpdated NewVersion VersionRange
  |
  Unmodified PackageName
  deriving stock (Eq, Show, Generic)

changed' :: [BuildSuccess] -> [Candidate]
changed' =
  mapMaybe \case
    CandidateBuilt candidate -> Just candidate
    RangeUpdated version range -> Just Candidate {version, range = NewRange range}
    Unmodified _ -> Nothing

data BuildResult a =
  BuildResult {
    success :: [BuildSuccess],
    failed :: [DepMutation a],
    state :: ManagedState,
    removable :: Maybe RemovableBounds,
    fatal :: Maybe Text
  }
  deriving stock (Eq, Show)

emptyBuildResult :: ManagedState -> BuildResult a
emptyBuildResult state =
  BuildResult {
    success = [],
    failed = [],
    state,
    removable = Nothing,
    fatal = Nothing
  }

buildResultEmpty :: BuildResult a -> Bool
buildResultEmpty BuildResult {success, failed, fatal} =
  null success && null failed && isNothing fatal

data BuildResults a =
  BuildResults {
    envs :: Map EnvName (BuildResult a),
    state :: ManagedEnvState
  }
  deriving stock (Eq, Show, Generic)

data BuildState a s =
  BuildState {
    success :: [BuildSuccess],
    failed :: [DepMutation a],
    state :: ManagedState,
    ext :: s
  }
  deriving stock (Eq, Show)

buildResult ::
  RemovableBounds ->
  BuildState a s ->
  BuildResult a
buildResult RemovableBounds {targetBound, deps} BuildState {..} =
  BuildResult {fatal = Nothing, ..}
  where
    removable = Just RemovableBounds {targetBound, deps = filterRemovable deps}
    filterRemovable =
      Map.mapMaybe \ targetDeps ->
        case Set.intersection targetDeps successNames of
          [] -> Nothing
          added -> Just added

    successNames = Set.fromList (changed' success <&> (.version.package))

changed :: BuildResult a -> [Candidate]
changed BuildResult {success} =
  changed' success

unchanged :: BuildResult a -> [PackageName]
unchanged BuildResult {success} =
  flip mapMaybe success \case
    CandidateBuilt _ -> Nothing
    RangeUpdated _ _ -> Nothing
    Unmodified package -> Just package

initBuildResults :: ManagedEnvState -> BuildResults a
initBuildResults state =
  BuildResults {envs = mempty, state}

-- TODO EnvName in BuildResult?
updateBuildResults ::
  ManagedOp ->
  EnvName ->
  TargetDeps ->
  BuildResults a ->
  BuildResult a ->
  BuildResults a
updateBuildResults op env targetDeps pre result =
  BuildResults {
    envs = Map.insert env result pre.envs,
    state = envStateWithMutations op env targetDeps result.state pre.state
  }

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

buildOutput :: ManagedOp -> BuildResults a -> BuildOutput
buildOutput operation results =
  buildOutputFromLists operation modified unmodified failed
  where
    modified = sortOn (.version.package) (concatMap changed envResults)
    unmodified = sort (concatMap unchanged envResults)
    failed = sort ((.package) <$> concatMap (.failed) envResults)
    envResults = toList results.envs
