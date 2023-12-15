module Hix.Managed.Data.BuildOutput where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.Extra (nubOrd, nubOrdOn)
import qualified Data.Text as Text

import qualified Hix.Data.PackageId
import Hix.Data.PackageName (PackageName (PackageName))
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Data.BuildResult (failedMutations, successCandidates, unmodifiedDeps)
import qualified Hix.Managed.Data.BuildResults
import Hix.Managed.Data.BuildResults (BuildResults)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)
import Hix.Managed.Data.ManagedConfig (ManagedOp)

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
    modifiedNames = comma ((.package.name) <$> modified),
    unmodifiedNames = comma unmodified,
    failedNames = comma failed
  }
  where
    modified = sortOn (.package.name) (nubOrdOn (.package.name) modified')
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
    modified = sortOn (.package.name) (concatMap successCandidates envResults)
    unmodified = sort (concatMap unmodifiedDeps envResults)
    failed = sort ((.package) <$> concatMap failedMutations envResults)
    envResults = toList results.envs
