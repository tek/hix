module Hix.Managed.Data.RevisionConfig where

import Hix.Data.PackageName (PackageName)
import Hix.Managed.Git (BranchName)

data RevisionConfig =
  RevisionConfig {
    targets :: Maybe (NonEmpty (Either PackageName BranchName)),
    -- | Fetch tags etc.
    ci :: Bool
  }
  deriving stock (Eq, Show)

instance Default RevisionConfig where
  def =
    RevisionConfig {
      targets = Nothing,
      ci = False
    }
