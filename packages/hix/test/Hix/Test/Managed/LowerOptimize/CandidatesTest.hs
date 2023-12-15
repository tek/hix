module Hix.Test.Managed.LowerOptimize.CandidatesTest where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Distribution.Version (Version, orLaterVersion)
import Hedgehog (evalEither, (===))

import Hix.Data.Dep (mainDep)
import Hix.Data.Error (Error (Client))
import Hix.Data.LowerConfig (lowerConfigOptimize)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState (ManagedState))
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (NewRange (NewRange))
import Hix.Managed.Build.Mutation (MutationResult (MutationSuccess))
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import Hix.Managed.Data.ManagedConfig (ManagedOp (OpLowerOptimize))
import Hix.Managed.Data.SolverParams (BoundMutation (ExtendedBound), mutation)
import Hix.Managed.Handlers.Mutation.Lower (processMutationLower)
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers (SolveHandlers))
import Hix.Managed.Lower.Candidates (candidatesOptimize)
import Hix.Managed.Lower.Data.Lower (LowerState (..))
import Hix.Managed.Solve.Changes (SolverPlan (..))
import Hix.Monad (M, throwM)
import Hix.Test.Utils (UnitTest, runMTest)

availableVersions :: PackageName -> M [Version]
availableVersions = \case
  "dep" -> pure versions
  _ -> throwM (Client "No such package")
  where
    versions = s1 ++ s2
    s1 = [[1, m, n] | m <- [7 .. 9], n <- [1 .. 3]]
    s2 = [[2, m, n] | m <- [0 .. 4], n <- [1 .. 3]]

targets :: [Version]
targets =
  s1 ++ s2 ++ s3
  where
    s1 = [[1, 8, n] | n <- reverse [1 .. 3]]
    s2 = [[1, 9, n] | n <- reverse [1 .. 2]]
    s3 = [[2, m, n] | m <- [0 .. 3], n <- reverse [1 .. 3]]

test_candidatesOptimize :: UnitTest
test_candidatesOptimize = do
  buildRef <- liftIO (newIORef [])
  let
    solveForVersion _ _ nv = do
      liftIO (modifyIORef' buildRef (nv.version :))
      pure case nv.version of
        [1, 9, 2] -> Just SolverPlan {overrides = [nv], matching = []}
        _ -> Nothing
    solve = SolveHandlers {solveForVersion}
  result <- liftIO $ runMTest False do
    majors <- candidatesOptimize availableVersions dep
    for majors \ mut ->
      processMutationLower solve OpLowerOptimize lowerConfigOptimize state mut build
  mutationResults <- evalEither result
  Just (MutationSuccess candidate mstate newState) === mutationResults
  triedVersions <- liftIO (readIORef buildRef)
  targets === triedVersions
  where
    dep = mainDep package (orLaterVersion ([2, 4]))

    package = "dep"

    build _ = pure (Just mstate)

    mstate = ManagedState {bounds = mempty, overrides = mempty}

    newState = LowerState {solverParams = [("dep", mempty {mutation = ExtendedBound candidateVersion})]}

    state = LowerState {solverParams = [("dep", mempty)]}

    candidate =
      Candidate {
        package = PackageId {name = package, version = candidateVersion},
        range = NewRange (orLaterVersion candidateVersion)
      }

    candidateVersion = [1, 9, 2]
