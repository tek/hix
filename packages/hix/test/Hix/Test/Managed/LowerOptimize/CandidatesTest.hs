module Hix.Test.Managed.LowerOptimize.CandidatesTest where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Distribution.Version (Version, orLaterVersion)
import Hedgehog (evalEither, (===))
import Path (absdir)

import Hix.Data.Dep (mainDep)
import Hix.Data.Error (Error (Client))
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState (ManagedState))
import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.Package (PackageName)
import Hix.Data.Version (NewRange (NewRange), NewVersion (..))
import Hix.Managed.Build.Mutation (MutationResult (MutationSuccess))
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import Hix.Managed.Data.SolverBounds (SolverBound (ExtendedBound, NoBounds))
import qualified Hix.Managed.Handlers.Build as BuildHandlers
import Hix.Managed.Handlers.Build (BuildHandlers (..))
import Hix.Managed.Handlers.Mutation.LowerOptimize (processMutationLowerOptimize)
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers (SolveHandlers))
import Hix.Managed.Lower.Candidates (candidatesOptimize)
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimizeState (..))
import Hix.Managed.Solve.Changes (SolverPlan (..))
import Hix.Monad (M, runMWith, throwM)
import Hix.Test.Utils (UnitTest)

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
    solveForVersion _ nv = do
      liftIO (modifyIORef' buildRef (nv.version :))
      pure case nv.version of
        [1, 9, 2] -> Just SolverPlan {configured = [nv], preexisting = []}
        _ -> Nothing
    handlers = BuildHandlers.handlersNull {solve = SolveHandlers {solveForVersion}}
  result <- liftIO $ runMWith False False True OutputNone [absdir|/project|] do
    majors <- candidatesOptimize availableVersions dep
    for majors \ mut ->
      processMutationLowerOptimize handlers state mut build
  mutationResults <- evalEither result
  Just (MutationSuccess candidate mstate newState) === mutationResults
  triedVersions <- liftIO (readIORef buildRef)
  targets === triedVersions
  where
    dep = mainDep package (orLaterVersion ([2, 4]))

    package = "dep"

    build _ = pure (Just mstate)

    mstate = ManagedState {bounds = mempty, overrides = mempty}

    newState = LowerOptimizeState {solverBounds = [("dep", ExtendedBound candidateVersion)]}

    state = LowerOptimizeState {solverBounds = [("dep", NoBounds)]}

    candidate =
      Candidate {
        version = NewVersion {package, version = candidateVersion},
        range = NewRange (orLaterVersion candidateVersion)
      }

    candidateVersion = [1, 9, 2]
