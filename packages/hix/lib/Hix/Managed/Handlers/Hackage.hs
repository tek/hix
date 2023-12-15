module Hix.Managed.Handlers.Hackage where

import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (SourceHash (SourceHash))
import Hix.Pretty (showP)

data HackageHandlers =
  HackageHandlers {
    fetchHash :: PackageId -> M SourceHash
  }

handlersNull :: HackageHandlers
handlersNull =
  HackageHandlers {
    fetchHash = \ package -> pure (SourceHash (showP package))
  }
