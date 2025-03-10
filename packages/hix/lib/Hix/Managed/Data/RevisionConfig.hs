module Hix.Managed.Data.RevisionConfig where

import Hix.Data.PackageName (PackageName)
import Hix.Managed.Git (BranchName)

data RevisionConfig =
  RevisionConfig {
    targets :: Maybe (NonEmpty (Either PackageName BranchName)),
    -- | Fetch tags and branches.
    fetch :: Bool,
    -- | Use the global git config rather than a synthetic committer ID (hix@tryp.io).
    globalGit :: Bool
  }
  deriving stock (Eq, Show)

instance Default RevisionConfig where
  def =
    RevisionConfig {
      targets = Nothing,
      fetch = False,
      globalGit = False
    }
