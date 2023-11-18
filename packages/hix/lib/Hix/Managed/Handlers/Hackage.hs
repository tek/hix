module Hix.Managed.Handlers.Hackage where

import Distribution.Version (Version)
import Exon (exon)

import Hix.Data.Package (PackageName)
import Hix.Data.Version (SourceHash (SourceHash))
import Hix.Data.Monad (M)
import Hix.Pretty (showP)

data HackageHandlers =
  HackageHandlers {
    fetchHash :: PackageName -> Version -> M SourceHash
  }

handlersNull :: HackageHandlers
handlersNull =
  HackageHandlers {
    fetchHash = \ package version -> pure (SourceHash [exon|##{package}-#{showP version}|])
  }
