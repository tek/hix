module Hix.Managed.Lower.Build where

import Distribution.Pretty (Pretty)
import Exon (exon)

import Hix.Data.Dep (Dep)
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Log as Log
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.BuildResult (BuildResult, buildResult)
import qualified Hix.Managed.Data.BuildState
import Hix.Managed.Data.BuildState (BuildState (BuildState), failed)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedJob as ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob, deps)
import qualified Hix.Managed.Data.SolverParams as SolverParams
import Hix.Managed.Data.SolverParams (SolverParams)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (Builder, EnvBuilder)
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import Hix.Managed.Lower.Data.Lower (LowerState (LowerState))

convergeMutations ::
  Pretty a =>
  (EnvBuilder -> M (MutationHandlers a s)) ->
  ManagedApp ->
  LowerConfig ->
  Builder ->
  ManagedJob ->
  ManagedState ->
  s ->
  [DepMutation a] ->
  M (BuildState a s)
convergeMutations mkMutationHandlers app conf builder job initialState initExt initialMutations =
  spin False (BuildState {success = [], failed = initialMutations, state = initialState, ext = initExt}) 1
  where
    spin noNewSuccess acc iteration

      | noNewSuccess
      = pure acc

      | [] <- acc.failed
      = pure acc

      | iteration > conf.maxIterations
      = pure acc

      | otherwise
      = do
        Log.debug [exon|Iteration #{show iteration} for '##{job.env :: EnvName}'|]
        BuildState {success, ..} <- build acc
        spin (length success == length acc.success) BuildState {failed = reverse failed, ..} (iteration + 1)

    build BuildState {failed = mutations, ..} =
      buildMutations app.build.hackage builder mkMutationHandlers job mutations BuildState {failed = [], ..}

-- TODO refactor the concept from lowerInit that restricts processing to a subset of deps, since we might want to make
-- it possible to control that via CLI for all commands.
lowerJob ::
  Pretty a =>
  ([Dep] -> SolverParams) ->
  (Dep -> M (Maybe (DepMutation a))) ->
  (EnvBuilder -> M (MutationHandlers a LowerState)) ->
  ManagedApp ->
  LowerConfig ->
  Builder ->
  ManagedJob ->
  M (BuildResult a)
lowerJob initialBounds candidates mkMutationHandlers app conf builder job = do
  mutations <- catMaybes <$> traverse candidates job.deps
  buildResult job.removable <$> convergeMutations mkMutationHandlers app conf builder job initialState ext mutations
  where
    ext = LowerState solverBounds
    solverBounds = SolverParams.fromConfig app.solverBounds <> initialBounds job.deps
    initialState = ManagedJob.initialState job
