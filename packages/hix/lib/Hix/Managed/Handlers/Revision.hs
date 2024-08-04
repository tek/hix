module Hix.Managed.Handlers.Revision where

import Hix.Managed.Git (GitApi)
import Hix.Managed.Handlers.HackageClient (HackageClient)
import Hix.Managed.Maint.Git (GitRevision)

data RevisionHandlers =
  RevisionHandlers {
    git :: GitApi GitRevision,
    publishHackages :: NonEmpty HackageClient
  }
