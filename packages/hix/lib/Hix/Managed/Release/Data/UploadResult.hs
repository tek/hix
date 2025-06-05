module Hix.Managed.Release.Data.UploadResult where

import Data.List.Extra (nubOrd)
import GHC.IsList (IsList (..))

import Hix.Class.Map (LookupMaybe, NMap, nAny, nConcatWith, nKeys, nNull, (!?))
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Cabal.Data.UploadStage (ArtifactSort (..), UploadMutability (..), UploadStage (..))
import Hix.Managed.Data.UploadResult (ArtifactResult, RepoResult (..), artifactResultFailed)

-- | Upload results for all stages for a single package.
-- Missing entries mean the stage was not requested.
newtype PackageResult =
  PackageResult (Map UploadStage ArtifactResult)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Monoid, Semigroup, IsList)

instance NMap PackageResult UploadStage ArtifactResult LookupMaybe

-- | Check if any stage had failures.
hasFailure :: PackageResult -> Bool
hasFailure =
  nAny artifactResultFailed

-- | Check if publishing sources succeeded on any repo.
publishSucceeded :: PackageResult -> Bool
publishSucceeded result =
  maybe False (nAny isSuccess) (result !? publishSourcesStage)
  where
    isSuccess = \case
      RepoSuccess {} -> True
      RepoFailed {} -> False
    publishSourcesStage = UploadStage {artifact = ArtifactSources, mutability = UploadPublish}

-- | Check if an upload was attempted for a stage (exists and non-empty).
wasAttempted :: PackageResult -> UploadStage -> Bool
wasAttempted result stage =
  maybe False (not . nNull) (result !? stage)

-- | Get all repo names that appear in any stage result.
allRepos :: PackageResult -> [HackageName]
allRepos =
  nubOrd . nConcatWith (const nKeys)
