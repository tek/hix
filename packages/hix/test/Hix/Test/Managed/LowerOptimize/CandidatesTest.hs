module Hix.Test.Managed.LowerOptimize.CandidatesTest where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Distribution.Version (Version)
import Hedgehog (evalEither, (===))

import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import qualified Hix.Data.VersionBounds
import Hix.Data.VersionBounds (fromLower, fromUpper)
import qualified Hix.Managed.Cabal.Data.SolverState
import Hix.Managed.Cabal.Data.SolverState (SolverState (SolverState), solverState, updateSolverState)
import Hix.Managed.Data.Constraints (MutationConstraints (MutationConstraints, mutation))
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (BuildMutation (BuildMutation), MutationResult (MutationSuccess))
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState (MutationState))
import qualified Hix.Managed.Handlers.AvailableVersions as AvailableVersions
import Hix.Managed.Handlers.AvailableVersions (AvailableVersionsHandlers (..))
import Hix.Managed.Handlers.Mutation.Lower (processMutationLower)
import Hix.Managed.Lower.Candidates (candidatesOptimize)
import Hix.Managed.Lower.Data.LowerMode (lowerOptimizeMode)
import Hix.Managed.Lower.Optimize (lowerOptimizeUpdate)
import Hix.Managed.QueryDep (simpleQueryDep)
import Hix.Monad (M, clientError)
import Hix.Test.Run (runMTestDir)
import Hix.Test.Utils (UnitTest)

fetchVersions :: PackageName -> M [Version]
fetchVersions = \case
  "dep" -> pure versions
  _ -> clientError "No such package"
  where
    versions = s1 ++ s2
    s1 = [[1, m, n] | m <- [7 .. 9], n <- [1 .. 3]]
    s2 = [[2, m, n] | m <- [0 .. 4], n <- [1 .. 3]]

availableVersions :: AvailableVersionsHandlers
availableVersions =
  AvailableVersions.handlersActionAll fetchVersions

targets :: [Version]
targets =
  s1 ++ s2 ++ s3
  where
    s1 = [[1, 8, n] | n <- reverse [1 .. 3]]
    s2 = [[1, 9, n] | n <- reverse [1 .. 2]]
    s3 = [[2, m, n] | m <- [0 .. 3], n <- reverse [1 .. 3]]

candidateVersion :: Version
candidateVersion = [1, 9, 2]

build :: IORef [Maybe Version] -> BuildMutation -> M (Maybe (MutationState, Set PackageId))
build buildRef BuildMutation {solverState = SolverState {constraints = [("dep", MutationConstraints {mutation})]}} = do
  liftIO (modifyIORef' buildRef (mutation.lower :))
  pure do
    s <- result =<< mutation.lower
    pure (s, [])
  where
    result version
      | candidateVersion == version
      = Just MutationState {
        bounds = [("dep", fromLower version)],
        versions = [("dep", Just version)],
        overrides = mempty,
        initial = []
      }
      | otherwise
      = Nothing
build _ _ = pure Nothing

test_candidatesOptimize :: UnitTest
test_candidatesOptimize = do
  buildRef <- liftIO (newIORef [])
  let
  result <- liftIO $ runMTestDir def do
    majors <- candidatesOptimize availableVersions mempty dep
    for majors \ mut ->
      processMutationLower def lowerOptimizeMode lowerOptimizeUpdate initialState mut (build buildRef)
  mutationResults <- evalEither result
  Just (MutationSuccess candidate True mstate [] (updateSolverState (const newConstraints) initialState)) === mutationResults
  triedVersions <- liftIO (readIORef buildRef)
  (Just <$> targets) === triedVersions
  where
    dep = simpleQueryDep mutable [[2, 4]]

    mstate =
      MutationState {
        bounds = [("dep", fromLower candidateVersion)],
        versions = [(mutable, Just candidateVersion)],
        overrides = mempty,
        initial = []
      }

    newConstraints = [(package, mempty {mutation = fromUpper candidateVersion})]

    initialState = solverState mempty mempty constraints def

    constraints = [(package, mempty)]

    candidate = MutableId {name = mutable, version = candidateVersion}

    mutable :: MutableDep
    mutable = "dep"

    package = "dep"
