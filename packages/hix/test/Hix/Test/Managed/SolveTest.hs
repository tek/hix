module Hix.Test.Managed.SolveTest where

import Hedgehog (evalEither)

import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import Hix.Managed.Cabal.Data.SolverState (solverState)
import qualified Hix.Managed.Cabal.Resources as SolveResources
import Hix.Managed.Cabal.Solve (solveWithCabal)
import Hix.Managed.Data.Constraints (MutationConstraints (..))
import Hix.Managed.Data.EnvContext (EnvDeps (EnvDeps))
import Hix.Pretty (showP)
import Hix.Test.Managed.UnsafeIsString ()
import Hix.Test.Utils (UnitTest, runMTest)

test_solve :: UnitTest
test_solve =
  evalEither =<< liftIO do
    runMTest True do
      solveResources <- SolveResources.acquire mempty def (GhcDbSystem Nothing)
      result <- solveWithCabal solveResources state
      liftIO do
        putStrLn (showP constraints)
        putStrLn (maybe "no plan" showP result)
  where
    state = solverState [] (EnvDeps ["containers", "text"] mempty) constraints def
    constraints =
      [
        ("text", mempty {mutation = ">=2.1.1"}),
        ("containers", mempty)
      ]
