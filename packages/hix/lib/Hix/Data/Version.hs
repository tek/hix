module Hix.Data.Version where

import Data.Aeson (FromJSON)
import Distribution.Version (Version, VersionRange)

import Hix.Data.ComponentConfig (PackageName)

data NewRange =
  NewRange {
    range :: VersionRange
  }
  deriving stock (Eq, Show, Generic)

data VersionBump =
  VersionBump {
    package :: PackageName,
    newVersion :: Version,
    range :: Maybe NewRange
  }
  deriving stock (Eq, Show, Generic)

newtype SourceHash =
  SourceHash Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, FromJSON)

data VersionBumped =
  VersionBumped {
    bump :: VersionBump,
    hash :: SourceHash
  }
  deriving stock (Eq, Show, Generic)
