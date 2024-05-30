module Hix.Managed.Data.Query where

import GHC.Exts (IsList)

import Hix.Data.PackageName (PackageName)
import Hix.Managed.Data.QueryDep (QueryDep)

-- | User-selected restriction of packages that should be processed.
-- Given as positional arguments on the command line.
-- If empty, all packages are processed.
newtype RawQuery =
  RawQuery [PackageName]
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsList)

-- | Stage-specific subset of the validated environment subset of 'RawQuery', adorned with metadata.
newtype Query =
  Query (NonEmpty QueryDep)
  deriving stock (Eq, Show, Generic)
