module Hix.Managed.Lower.Build where

import Distribution.Pretty (Pretty)
import Exon (exon)

import Hix.Data.Dep (Dep)
import Hix.Data.Deps (ProjectDep)
import Hix.Data.EnvName (EnvName)
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import Hix.Deps (uniqueDeps)
import qualified Hix.Log as Log
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Mutation (DepMutation)
import qualified Hix.Managed.Data.BuildDomain
import Hix.Managed.Data.BuildDomain (BuildDomain)
import Hix.Managed.Data.BuildResult (BuildResult, buildResult)
import qualified Hix.Managed.Data.BuildState
import Hix.Managed.Data.BuildState (BuildState (BuildState), failed)
import qualified Hix.Managed.Data.ManagedApp
import Hix.Managed.Data.ManagedApp (ManagedApp)
import qualified Hix.Managed.Data.ManagedJob as ManagedJob
import Hix.Managed.Data.ManagedJob (ManagedJob)
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
  BuildDomain ->
  ManagedState ->
  s ->
  [DepMutation a] ->
  M (BuildState a s)
convergeMutations mkMutationHandlers app conf builder domain initialState initExt initialMutations =
  spin (BuildState {success = [], failed = initialMutations, state = initialState, ext = initExt}) 1
  where
    spin acc iteration

      | [] <- acc.failed
      = pure acc

      | iteration > conf.maxIterations
      = pure acc

      | otherwise
      = do
        Log.debug [exon|Iteration #{show iteration} for '##{domain.env :: EnvName}'|]
        BuildState {success, ..} <- build acc
        let newState = BuildState {failed = reverse failed, ..}
        if length success == length acc.success
        then pure newState
        else spin newState (iteration + 1)

    build BuildState {failed = mutations, ..} =
      buildMutations app.build.hackage builder mkMutationHandlers domain mutations BuildState {failed = [], ..}

lowerMutations ::
  Pretty a =>
  (Dep -> M (Maybe (DepMutation a))) ->
  (EnvBuilder -> M (MutationHandlers a LowerState)) ->
  ManagedApp ->
  LowerConfig ->
  Builder ->
  BuildDomain ->
  ManagedState ->
  SolverParams ->
  M (BuildState a LowerState)
lowerMutations candidates mkMutationHandlers app conf builder domain initialState solverParams = do
  mutations <- catMaybes <$> traverse candidates domain.query
  convergeMutations mkMutationHandlers app conf builder domain initialState ext mutations
  where
    ext = LowerState solverParams

-- TODO refactor the concept from lowerInit that restricts processing to a subset of deps, since we might want to make
-- it possible to control that via CLI for all commands.
lowerJob ::
  Pretty a =>
  ([ProjectDep] -> SolverParams) ->
  (Dep -> M (Maybe (DepMutation a))) ->
  (EnvBuilder -> M (MutationHandlers a LowerState)) ->
  ManagedApp ->
  LowerConfig ->
  Builder ->
  ManagedJob ->
  M (BuildResult a)
lowerJob initialParams candidates mkMutationHandlers app conf builder job = do
  finalState <- lowerMutations candidates mkMutationHandlers app conf builder job.domain initialState solverParams
  pure (buildResult job.removable finalState)
  where
    solverParams =
      SolverParams.fromUserConfig app.solverBounds <>
      initialParams (uniqueDeps job.targetDeps)

    initialState = ManagedJob.initialState job
