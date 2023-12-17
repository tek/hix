module Hix.Managed.Handlers.Lower where

import Hix.Data.Monad (M)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Data.ManagedPackage (ManagedPackages)
import qualified Hix.Managed.Handlers.Build as Build
import Hix.Managed.Handlers.Build (BuildHandlers, EnvBuilder)
import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import qualified Hix.Managed.Handlers.Solve as Solve
import Hix.Managed.Handlers.Solve (SolveHandlers)
import Hix.Managed.Lower.Data.Lower (Lower)

data LowerHandlers =
  LowerHandlers {
    build :: BuildHandlers,
    solve :: ManagedPackages -> EnvBuilder -> M SolveHandlers,
    report :: ReportHandlers Lower,
    versions :: PackageName -> M [Version]
  }

data SpecialLowerHandlers =
  TestLowerHandlers
  deriving stock (Eq, Show)

handlersNull :: LowerHandlers
handlersNull =
  LowerHandlers {
    build = Build.handlersNull,
    solve = \ _ _ -> pure Solve.handlersNull,
    report = Report.handlersNull,
    versions = \ _ -> pure []
  }
