module Hix.Managed.Release.Data.ReleaseResult where

import Data.Aeson (ToJSON)

import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Git (BranchName)
import Hix.Managed.Release.Data.UploadResult (PackageResult)

-- | A package that was successfully uploaded (possibly with partial failures).
-- Uses 'PackageResult' for upload results, which maps upload stage to artifact results.
data ReleasedPackage =
  ReleasedPackage {
    package :: PackageId,
    results :: PackageResult
  }
  deriving stock (Eq, Show, Generic)

-- | A package that failed during preparation or upload.
data FailedPackage =
  FailedPackage {
    package :: PackageId,
    reason :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | A package that was not selected for release.
data SkippedPackage =
  SkippedPackage {
    name :: LocalPackage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Per-package status for release output.
data PackageStatus =
  StatusReleased ReleasedPackage
  |
  StatusFailed FailedPackage
  |
  StatusSkipped SkippedPackage
  deriving stock (Eq, Show, Generic)

data ReleaseOutput =
  ReleaseOutput {
    success :: Bool,
    title :: Text,
    message :: Text,
    branch :: Maybe BranchName
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

filterReleased :: [PackageStatus] -> [ReleasedPackage]
filterReleased = mapMaybe \case
  StatusReleased pkg -> Just pkg
  _ -> Nothing

isFailure :: PackageStatus -> Bool
isFailure = \case
  StatusFailed {} -> True
  _ -> False
