module Hix.Managed.Release.Data.GitResult where

import qualified Data.Set as Set

import Hix.Managed.Git (BranchName, Tag)

-- | Summary of git operations performed during the release bracket.
data GitResult =
  GitResult {
    -- | The temporary release branch and commit description, if a commit was created.
    committed :: Maybe (BranchName, Text),
    -- | Tags that were created.
    tagged :: Set Tag,
    -- | The branch that was active before the release.
    initialBranch :: Maybe BranchName,
    -- | The initial branch name, if the release branch was merged into it.
    merged :: Maybe BranchName,
    -- | The remote name, if refs were pushed.
    pushed :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

noGitResult :: GitResult
noGitResult =
  GitResult {committed = Nothing, tagged = Set.empty, initialBranch = Nothing, merged = Nothing, pushed = Nothing}
