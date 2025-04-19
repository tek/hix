module Hix.Integration.Managed.SolveTest where

import qualified Data.Set as Set
import Hedgehog (evalEither)

import Hix.Class.Map (nKeys)
import Hix.Integration.Utils (UnitTest)
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Changes (solverPlan)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSystem))
import Hix.Managed.Cabal.Data.SolverState (solverState)
import qualified Hix.Managed.Cabal.Resources as SolveResources
import Hix.Managed.Cabal.Solve (solveWithCabal')
import Hix.Managed.Cabal.Sort (sortDeps)
import Hix.Managed.Data.EnvContext (EnvDeps (EnvDeps))
import Hix.Managed.Data.Initial (Initial (..))
import Hix.Managed.Data.Mutable (unsafeMutableDep)
import Hix.Pretty (prettyL, showP, showPL)
import Hix.Test.Run (logConfigDebug, runMTestDir)

test_solve :: UnitTest
test_solve =
  evalEither =<< liftIO do
    runMTestDir logConfigDebug do
      (res, _) <- SolveResources.acquire mempty def (GhcDbSystem Nothing)
      mb_sip <- solveWithCabal' res state
      sorted <- sortDeps res id (nKeys constraints)
      liftIO do
        putStrLn ""
        putStrLn "Constraints:"
        putStrLn (showP constraints)
        for_ mb_sip \ sip -> do
          let plan = solverPlan sip
          putStrLn ""
          putStrLn "Changes:"
          print (prettyL plan.changes)
          putStrLn ""
          putStrLn "Matching DB:"
          print (prettyL plan.matching)
          putStrLn ""
        putStrLn "Sorted:"
        putStrLn (showPL sorted)
  where
    Initial state = solverState [] (EnvDeps (Set.fromList (unsafeMutableDep <$> nKeys constraints))) constraints def
    constraints =
      [
        ("ansi-terminal", mempty),
        ("async", mempty),
        ("polysemy-conc", mempty),
        ("polysemy-time", mempty),
        ("stm", mempty),
        ("time", mempty)
      ]
