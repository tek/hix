module Hix.Managed.Handlers.Solve.Test where

import Hix.Data.PackageId (PackageId)
import Hix.Managed.Handlers.Solve (SolveHandlers (..))
import Hix.Managed.Solve (solveWithCabal)
import Hix.Managed.Solve.Mock (mockSolveResources)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages)

testHandlers ::
  [(PackageId, [PackageId])] ->
  SourcePackages ->
  SolveHandlers
testHandlers installed available =
  SolveHandlers {solveForVersion = solveWithCabal (mockSolveResources installed available)}
