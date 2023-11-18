module Hix.Managed.Handlers.LowerInit where

import Distribution.Version (Version)

import Hix.Data.Package (PackageName)
import qualified Hix.Managed.Handlers.Build as Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import Hix.Managed.Lower.Data.LowerInit (LowerInit)
import Hix.Monad (M)

data LowerInitHandlers =
  LowerInitHandlers {
    build :: BuildHandlers,
    report :: ReportHandlers LowerInit,
    versions :: PackageName -> M [Version]
  }

data SpecialLowerInitHandlers =
  TestLowerInitHandlers
  deriving stock (Eq, Show)

handlersNull :: LowerInitHandlers
handlersNull =
  LowerInitHandlers {
    build = Build.handlersNull,
    report = Report.handlersNull,
    versions = \ _ -> pure []
  }
