module Hix.Managed.Lower.Build where

import Distribution.Pretty (Pretty)

import Hix.Data.Dep (Dep)
import Hix.Data.Monad (M)
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.Build (BuildResult)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob, deps)
import qualified Hix.Managed.Data.SolverParams as SolverParams
import Hix.Managed.Data.SolverParams (SolverParams)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (Builder, EnvBuilder)
import Hix.Managed.Handlers.Mutation (MutationHandlers)

-- TODO refactor the concept from lowerInit that restricts processing to a subset of deps, since we might want to make
-- it possible to control that via CLI for all commands.
-- Right now we omit skipped deps from the final overrides (not bounds I think) (which happens when there are no
-- candidates available).
-- WAIT this is not true, the overrides should be decided by the solver.
-- As long as the skipped candidates are in the deps, they should be included.
-- The versions of the skipped ones need to be copied to the initial solver bounds.
lowerJob ::
  Pretty a =>
  ([Dep] -> SolverParams) ->
  (SolverParams -> s) ->
  (Dep -> M (Maybe (DepMutation a))) ->
  (EnvBuilder -> M (MutationHandlers a s)) ->
  ManagedApp ->
  Builder ->
  ManagedJob ->
  M (BuildResult a)
lowerJob initialBounds consState candidates mkMutationHandlers app builder job = do
  mutations <- catMaybes <$> traverse candidates job.deps
  buildMutations app.build.hackage builder mkMutationHandlers job job.state mutations ext
  where
    ext = consState solverBounds
    solverBounds = SolverParams.fromConfig app.solverBounds <> initialBounds job.deps
