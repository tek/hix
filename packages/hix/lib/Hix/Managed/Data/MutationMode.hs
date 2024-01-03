module Hix.Managed.Data.MutationMode where

data MutationMode =
  MutationMode {
    foo :: Int
  }
  deriving stock (Eq, Show, Generic)
