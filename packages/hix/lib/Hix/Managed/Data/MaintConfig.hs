module Hix.Managed.Data.MaintConfig where

import Hix.Data.PackageName (PackageName)

data MaintConfig =
  MaintConfig {
    targets :: Maybe (NonEmpty PackageName),
    noFailures :: Bool,
    commit :: Bool,
    push :: Bool,
    -- | Create commits on new, timestamped, branches; so that a CI workflow may create pull requests.
    -- Omit publishing revisions.
    pr :: Bool,
    revision :: Bool,
    -- | Fetch tags and branches.
    fetch :: Bool,
    -- | Use the global git config rather than a synthetic committer ID (hix@tryp.io).
    globalGit :: Bool
  }
  deriving stock (Eq, Show)

instance Default MaintConfig where
  def =
    MaintConfig {
      targets = Nothing,
      noFailures = False,
      commit = False,
      push = False,
      pr = False,
      revision = False,
      fetch = False,
      globalGit = False
    }
