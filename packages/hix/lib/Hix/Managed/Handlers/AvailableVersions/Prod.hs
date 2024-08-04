module Hix.Managed.Handlers.AvailableVersions.Prod where

import Hix.Hackage.Versions (latestVersionHackage, versionsHackage)
import Hix.Managed.Handlers.AvailableVersions (AvailableVersionsHandlers (..))
import Hix.Managed.Handlers.HackageClient (HackageClient)

handlersProd :: NonEmpty HackageClient -> AvailableVersionsHandlers
handlersProd hackages =
  AvailableVersionsHandlers {
    all = versionsHackage hackages,
    latest = latestVersionHackage hackages
  }
