module Hix.Test.Managed.SolverTest where

import Hedgehog (evalEither)

import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Managed.Data.CabalTarget (CabalTarget, cabalTarget)
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpLowerInit))
import Hix.Managed.Solve (solveTargets)
import Hix.Managed.Solve.Mock (mockSolveResources)
import Hix.Managed.Solve.Mock.SourcePackage (SourcePackages)
import Hix.Managed.Solve.Print (printPlan)
import Hix.Monad (eitherFatal)
import Hix.Test.Utils (UnitTest, runMTest)

targets :: [CabalTarget]
targets =
  [
    cabalTarget OpLowerInit "pack1" mempty,
    cabalTarget OpLowerInit "pack2" mempty
  ]

installed :: [(PackageId, [PackageId])]
installed =
  [
    (PackageId {name = "pack1", version = [1, 0]}, [])
  ]

available :: SourcePackages
available =
  [
    ("pack1", [([1, 0], [])]),
    ("pack2", [([1, 0], [])])
  ]

test_solver :: UnitTest
test_solver = do
  res <- liftIO $ runMTest True do
    let resources = mockSolveResources installed available
    plan <- eitherFatal "solve" . first show =<< solveTargets resources targets
    printPlan plan
  evalEither res
