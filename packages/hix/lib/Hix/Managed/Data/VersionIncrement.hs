module Hix.Managed.Data.VersionIncrement where

data VersionIncrement =
  Patch
  |
  Minor
  |
  Major
  |
  Supermajor
  deriving stock (Eq, Show, Enum, Bounded, Ord)
