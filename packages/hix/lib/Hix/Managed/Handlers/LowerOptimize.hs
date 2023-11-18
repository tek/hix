module Hix.Managed.Handlers.LowerOptimize where

import Distribution.Version (Version)

import Hix.Data.Package (PackageName)
import qualified Hix.Managed.Handlers.Build as Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize)
import Hix.Data.Monad (M)

data LowerOptimizeHandlers =
  LowerOptimizeHandlers {
    build :: BuildHandlers,
    report :: ReportHandlers LowerOptimize,
    versions :: PackageName -> M [Version]
  }

data SpecialLowerOptimizeHandlers =
  TestLowerOptimizeHandlers
  deriving stock (Eq, Show)

handlersNull :: LowerOptimizeHandlers
handlersNull =
  LowerOptimizeHandlers {
    build = Build.handlersNull,
    report = Report.handlersNull,
    versions = \ _ -> pure []
  }
