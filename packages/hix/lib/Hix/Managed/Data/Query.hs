module Hix.Managed.Data.Query where

import GHC.Exts (IsList)

import Hix.Data.PackageName (PackageName)
import Hix.Managed.Data.QueryDep (QueryDep)

newtype RawQuery =
  RawQuery [PackageName]
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsList)

newtype Query =
  Query (NonEmpty QueryDep)
  deriving stock (Eq, Show, Generic)
