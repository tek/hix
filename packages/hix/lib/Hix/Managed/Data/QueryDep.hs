module Hix.Managed.Data.QueryDep where

import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Data.Mutable (MutableDep)

-- | A dep that is eligible for being processed, which requires that it is not referring to a project-local package,
-- that it wasn't excluded by CLI options and arbitrary other conditions specific to individual stages.
data QueryDep =
  QueryDep {
    package :: MutableDep,
    -- | Used to constrain the spectrum of candidate versions in some modes.
    version :: VersionBounds,
    installed :: Maybe Version,
    override :: Maybe Version
  }
  deriving stock (Eq, Show, Generic)
