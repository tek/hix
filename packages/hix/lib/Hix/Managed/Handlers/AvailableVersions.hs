module Hix.Managed.Handlers.AvailableVersions where

import Hix.Data.Monad (M)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)

data AvailableVersionsHandlers =
  AvailableVersionsHandlers {
    all :: PackageName -> M [Version],
    latest :: PackageName -> M (Maybe Version)
  }

handlersNull :: AvailableVersionsHandlers
handlersNull =
  AvailableVersionsHandlers {
    all = const (pure []),
    latest = const (pure Nothing)
  }

handlersActionAll :: (PackageName -> M [Version]) -> AvailableVersionsHandlers
handlersActionAll fetch =
  AvailableVersionsHandlers {
    all = fetch,
    latest = fmap (head . sortOn Down) . fetch
  }

handlersActionOne :: (PackageName -> M (Maybe Version)) -> AvailableVersionsHandlers
handlersActionOne fetch =
  AvailableVersionsHandlers {
    all = fmap toList . fetch,
    latest = fetch
  }
