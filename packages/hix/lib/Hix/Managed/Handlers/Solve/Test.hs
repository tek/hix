module Hix.Managed.Handlers.Solve.Test where

import Hix.Data.PackageId (PackageId)
import Hix.Managed.Data.ManagedPackage (ManagedPackages)
import Hix.Managed.Handlers.Solve (SolveHandlers (..))
import Hix.Managed.Solve (solveWithCabal)
import Hix.Managed.Solve.Mock (mockSolveResources)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages)

testHandlers ::
  ManagedPackages ->
  [(PackageId, [PackageId])] ->
  SourcePackages ->
  SolveHandlers
testHandlers packages installed available =
  SolveHandlers {solveForVersion = solveWithCabal (mockSolveResources packages installed available)}
