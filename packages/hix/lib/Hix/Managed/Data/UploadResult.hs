module Hix.Managed.Data.UploadResult where

import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (..))
import GHC.IsList (IsList (..))

import Hix.Class.Map (LookupMaybe, NMap, nAll)
import Hix.Data.Error (Error)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Pretty (prettyText)

newtype HackageUrl =
  HackageUrl Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

instance Pretty HackageUrl where
  pretty = prettyText . coerce

-- | Result of uploading a single artifact to a single Hackage repo.
data RepoResult =
  -- | Upload succeeded with the given URL.
  RepoSuccess HackageUrl
  |
  -- | Upload failed with the given error.
  RepoFailed Error
  deriving stock (Eq, Show, Generic)

-- | Results of uploading a single artifact to one or more Hackage repos.
-- Maps repo name to the result for that repo.
newtype ArtifactResult =
  ArtifactResult (Map HackageName RepoResult)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Monoid, Semigroup)

instance IsList ArtifactResult where
  type Item ArtifactResult = (HackageName, RepoResult)
  fromList = ArtifactResult . Map.fromList
  toList (ArtifactResult m) = Map.toList m

instance NMap ArtifactResult HackageName RepoResult LookupMaybe

-- | Check if all uploads succeeded (no failures).
artifactResultSuccess :: ArtifactResult -> Bool
artifactResultSuccess = nAll isSuccess
  where
    isSuccess = \case
      RepoSuccess {} -> True
      RepoFailed {} -> False

-- | Check if any upload failed.
artifactResultFailed :: ArtifactResult -> Bool
artifactResultFailed = not . artifactResultSuccess
