module Hix.Managed.Handlers.Lower where

import Distribution.Version (Version)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Package (PackageName)
import qualified Hix.Managed.Handlers.Build as Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import qualified Hix.Managed.Handlers.Solve as Solve
import Hix.Managed.Handlers.Solve (SolveHandlers)

data LowerHandlers a =
  LowerHandlers {
    build :: BuildHandlers,
    solve :: EnvName -> M SolveHandlers,
    report :: ReportHandlers a,
    versions :: PackageName -> M [Version]
  }

data SpecialLowerHandlers =
  TestLowerHandlers
  deriving stock (Eq, Show)

handlersNull :: LowerHandlers a
handlersNull =
  LowerHandlers {
    build = Build.handlersNull,
    solve = \ _ -> pure Solve.handlersNull,
    report = Report.handlersNull,
    versions = \ _ -> pure []
  }
