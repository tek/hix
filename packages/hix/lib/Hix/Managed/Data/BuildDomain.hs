module Hix.Managed.Data.BuildDomain where

import Hix.Data.Dep (Dep)
import Hix.Data.Deps (TargetRemoteDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Managed.Data.Targets (Targets)

data BuildDomain =
  BuildDomain {
    -- | The name of the hix env used to build this job.
    env :: EnvName,
    -- | The set of packages this job operates on.
    -- Sorted topologically by smart constructor.
    targets :: Targets,
    -- | The subset of deps on packages that aren't part of the project.
    deps :: TargetRemoteDeps,
    -- TODO in managedJob, filter these with the CLI options for selecting specific deps
    query :: [Dep]
  }
  deriving stock (Eq, Show, Generic)
