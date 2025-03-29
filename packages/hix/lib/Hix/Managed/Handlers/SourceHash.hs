module Hix.Managed.Handlers.SourceHash where

import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (SourceHash (SourceHash))
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Pretty (showP)

data SourceHashHandlers =
  SourceHashHandlers {
    fetchHash :: PackageId -> M (Either Text (SourceHash, Maybe HackageName))
  }

handlersNull :: SourceHashHandlers
handlersNull =
  SourceHashHandlers {
    fetchHash = \ package -> pure (Right (SourceHash (showP package), Nothing))
  }
