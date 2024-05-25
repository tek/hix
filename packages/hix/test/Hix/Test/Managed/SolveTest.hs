module Hix.Test.Managed.SolveTest where

import qualified Data.Set as Set
import Hedgehog (evalEither)

import Hix.Class.Map (nKeys)
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import Hix.Managed.Cabal.Data.SolverState (solverState)
import qualified Hix.Managed.Cabal.Resources as SolveResources
import Hix.Managed.Cabal.Solve (solveWithCabal)
import Hix.Managed.Data.Constraints (MutationConstraints (..))
import Hix.Managed.Data.EnvContext (EnvDeps (EnvDeps))
import Hix.Managed.Data.Mutable (unsafeMutableDep)
import Hix.Pretty (prettyL, showP)
import Hix.Test.Managed.UnsafeIsString ()
import Hix.Test.Utils (UnitTest, runMTest)

test_solve :: UnitTest
test_solve =
  evalEither =<< liftIO do
    runMTest True do
      solveResources <- SolveResources.acquire mempty def (GhcDbSystem Nothing)
      result <- solveWithCabal solveResources state
      liftIO do
        putStrLn ""
        putStrLn "Constraints:"
        putStrLn (showP constraints)
        putStrLn ""
        putStrLn "Changes:"
        putStrLn (maybe "no plan" (show . prettyL . (.changes)) result)
  where
    state = solverState [] (EnvDeps (Set.fromList (unsafeMutableDep <$> nKeys constraints)) mempty) constraints def
    constraints =
      [
        ("path", mempty {mutation = "==0.9.0"})
      ]
