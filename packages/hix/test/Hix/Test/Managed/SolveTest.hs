module Hix.Test.Managed.SolveTest where

import qualified Data.Set as Set
import Hedgehog (evalEither)

import Hix.Class.Map (nKeys)
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import Hix.Managed.Cabal.Data.SolverState (solverState)
import qualified Hix.Managed.Cabal.Resources as SolveResources
import Hix.Managed.Cabal.Solve (solveWithCabal)
import Hix.Managed.Cabal.Sort (sortDeps)
import Hix.Managed.Data.EnvContext (EnvDeps (EnvDeps))
import Hix.Managed.Data.Mutable (unsafeMutableDep)
import Hix.Pretty (prettyL, showP, showPL)
import Hix.Test.Managed.UnsafeIsString ()
import Hix.Test.Utils (UnitTest, runMTest)

test_solve :: UnitTest
test_solve =
  evalEither =<< liftIO do
    runMTest True do
      res <- SolveResources.acquire mempty def (GhcDbSystem Nothing)
      result <- solveWithCabal res state
      sorted <- sortDeps res id (nKeys constraints)
      liftIO do
        putStrLn ""
        putStrLn "Constraints:"
        putStrLn (showP constraints)
        putStrLn ""
        putStrLn "Changes:"
        putStrLn (maybe "no plan" (show . prettyL . (.changes)) result)
        putStrLn ""
        putStrLn "Sorted:"
        putStrLn (showPL sorted)
  where
    state = solverState [] (EnvDeps (Set.fromList (unsafeMutableDep <$> nKeys constraints)) mempty) constraints def
    constraints =
      [
        ("ansi-terminal", mempty),
        ("async", mempty),
        ("polysemy-conc", mempty),
        ("polysemy-time", mempty),
        ("stm", mempty),
        ("time", mempty)
      ]
