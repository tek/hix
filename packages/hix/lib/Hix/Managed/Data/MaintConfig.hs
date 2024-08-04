module Hix.Managed.Data.MaintConfig where

import Hix.Data.PackageName (PackageName)

data MaintConfig =
  MaintConfig {
    targets :: Maybe (NonEmpty PackageName),
    noFailures :: Bool,
    commit :: Bool,
    push :: Bool,
    revision :: Bool,
    -- | Fetch tags etc.
    ci :: Bool,
    -- | Create commits on new, timestamped, branches; so that a CI workflow may create pull requests.
    -- Omit publishing revisions.
    pr :: Bool
  }
  deriving stock (Eq, Show)

instance Default MaintConfig where
  def =
    MaintConfig {
      targets = Nothing,
      noFailures = False,
      commit = False,
      push = False,
      revision = False,
      ci = False,
      pr = False
    }
