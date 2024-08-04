module Hix.Managed.Maint.Data.MaintPrep where

import Hix.Managed.Git (BranchName, MaintBranch)

data MaintPrep =
  PrepBranch {
    targetBranch :: MaintBranch,
    workBranch :: BranchName
  }
  |
  PrepNoTags
  deriving stock (Eq, Show)
