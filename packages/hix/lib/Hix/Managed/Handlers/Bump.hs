module Hix.Managed.Handlers.Bump where

import Distribution.Version (Version)

import Hix.Data.Package (PackageName)
import qualified Hix.Managed.Handlers.Build as Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import Hix.Managed.Lower.Data.Bump (Bump)
import Hix.Monad (M)

data BumpHandlers =
  BumpHandlers {
    build :: BuildHandlers,
    report :: ReportHandlers Bump,
    latestVersion :: PackageName -> M (Maybe Version)
  }

data SpecialBumpHandlers =
  TestBumpHandlers
  deriving stock (Eq, Show)

handlersNull :: BumpHandlers
handlersNull =
  BumpHandlers {
    build = Build.handlersNull,
    report = Report.handlersNull,
    latestVersion = \ _ -> pure Nothing
  }
