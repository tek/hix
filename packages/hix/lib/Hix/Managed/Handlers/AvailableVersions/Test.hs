module Hix.Managed.Handlers.AvailableVersions.Test where

import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Managed.Cabal.Mock.SourcePackage (queryPackages, queryPackagesLatest)
import Hix.Managed.Handlers.AvailableVersions (AvailableVersionsHandlers (..))

handlersTest :: SourcePackages -> AvailableVersionsHandlers
handlersTest packages =
  AvailableVersionsHandlers {
    latest = queryPackagesLatest packages,
    all = queryPackages packages
  }
