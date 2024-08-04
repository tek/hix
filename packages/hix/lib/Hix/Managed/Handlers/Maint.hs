module Hix.Managed.Handlers.Maint where

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Managed.Data.BuildOutput (DepChanges)
import Hix.Managed.Git (GitApi)
import Hix.Managed.Handlers.Context (ContextHandlers)
import Hix.Managed.Handlers.HackageClient (HackageClient)
import Hix.Managed.Maint.Git (GitMaint)

data MaintHandlers =
  MaintHandlers {
    git :: GitApi GitMaint,
    context :: ContextHandlers,
    publishHackages :: NonEmpty HackageClient,
    runBump :: EnvName -> M DepChanges
  }
