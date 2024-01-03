module Hix.Managed.Build where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty)
import Exon (exon)
import Text.PrettyPrint (vcat)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (Version, Versions)
import Hix.Data.VersionBounds (VersionBounds)
import qualified Hix.Log as Log
import Hix.Managed.Build.Solve (solveMutation)
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Config (isNonReinstallableDep, isReinstallableId)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.Mutable (MutableDep, addBuildVersions)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (BuildMutation (BuildMutation), DepMutation, MutationResult (..))
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState (MutationState), updateBoundsWith)
import Hix.Managed.Data.Query (Query (Query))
import qualified Hix.Managed.Data.QueryDep
import Hix.Managed.Data.QueryDep (QueryDep)
import qualified Hix.Managed.Data.StageContext
import Hix.Managed.Data.StageContext (StageContext (StageContext))
import qualified Hix.Managed.Data.StageState
import Hix.Managed.Data.StageState (
  BuildStatus (Failure, Success),
  StageState,
  failed,
  initStageState,
  iterations,
  justSuccess,
  )
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (EnvBuilder)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import Hix.Managed.StageState (updateStageState)
import Hix.Pretty (prettyL, showP, showPL)

logBuildInputs ::
  EnvName ->
  Text ->
  [PackageId] ->
  M ()
logBuildInputs env description overrides = do
  Log.info [exon|Building targets in '##{env}' with #{description}...|]
  Log.debugP (vcat ["Overrides:", prettyL overrides])

logBuildResult :: Text -> BuildStatus -> M ()
logBuildResult description status =
  Log.info [exon|Build with ##{description} #{result status}|]
  where
    result = \case
      Success -> "succeeded"
      Failure -> "failed"

updateMutationState ::
  (Version -> VersionBounds -> VersionBounds) ->
  Versions ->
  Overrides ->
  MutationState ->
  MutationState
updateMutationState updateBound newVersions overrides MutationState {bounds, versions} =
  updateBoundsWith updateBound MutationState {
    bounds,
    versions = addBuildVersions newVersions versions,
    overrides
  }

-- | /Note/: This quietly discards non-reinstallable packages.
--
-- TODO Discard installed versions, for builds with initial versions.
buildVersions ::
  EnvBuilder ->
  EnvContext ->
  Text ->
  Versions ->
  [PackageId] ->
  M (Overrides, BuildStatus)
buildVersions builder context description versions overrideVersions = do
  logBuildInputs context.env description reinstallable
  (overrides, status) <- builder.buildWithState versions reinstallable
  logBuildResult description status
  pure (overrides, status)
  where
    reinstallable = filter isReinstallableId overrideVersions

buildConstraints ::
  EnvBuilder ->
  EnvContext ->
  Text ->
  EnvConstraints ->
  M (Maybe (Versions, Overrides, BuildStatus))
buildConstraints builder context description constraints =
  solveMutation builder.cabal context.deps constraints >>= traverse \ changes -> do
    (overrides, status) <- buildVersions builder context description changes.versions changes.overrides
    pure (changes.versions, overrides, status)

buildMutation ::
  EnvBuilder ->
  EnvContext ->
  MutationState ->
  BuildMutation ->
  M (Maybe MutationState)
buildMutation builder context state BuildMutation {description, constraints, updateBound} =
  result <$> buildConstraints builder context description constraints
  where
    result = \case
      Just (versions, overrides, status) ->
        justSuccess (updateMutationState updateBound versions overrides state) status
      Nothing -> Nothing

logMutationResult ::
  MutableDep ->
  MutationResult s ->
  M ()
logMutationResult package = \case
  MutationSuccess candidate _ _ ->
    Log.verbose [exon|Build succeeded for #{showP candidate}|]
  MutationKeep ->
    Log.verbose [exon|Build is up to date for '##{package}'|]
  MutationFailed ->
    Log.verbose [exon|Could not find a buildable version of '##{package}'|]

validateMutation ::
  EnvBuilder ->
  EnvContext ->
  MutationHandlers a s ->
  StageState a s ->
  DepMutation a ->
  M (StageState a s)
validateMutation envBuilder context handlers stageState mutation = do
  result <- processReinstallable
  logMutationResult mutation.package result
  pure (updateStageState stageState mutation result)
  where
    processReinstallable
      | isNonReinstallableDep mutation.package
      = pure MutationKeep
      | otherwise
      = handlers.process stageState.ext mutation build
    build = buildMutation envBuilder context stageState.state

convergeMutations ::
  Pretty a =>
  MutationHandlers a s ->
  BuildConfig ->
  EnvBuilder ->
  EnvContext ->
  StageState a s ->
  [DepMutation a] ->
  M (StageState a s)
convergeMutations handlers conf builder context state0 =
  spin state0 {iterations = 0}
  where
    spin state mutations

      | [] <- mutations
      = pure state

      | state.iterations >= conf.maxIterations
      = pure state

      | otherwise
      = do
        Log.debug [exon|Iteration #{show (state.iterations + 1)} for '##{context.env :: EnvName}'|]
        newState <- build state mutations
        if Map.size newState.success == Map.size state.success
        then pure newState
        -- reversing so the build order is consistent
        else spin newState (reverse newState.failed)

    build statePre mutations = do
      let state = statePre {failed = [], iterations = statePre.iterations + 1}
      Log.debug [exon|Building targets with mutations: #{showPL mutations}|]
      foldM (validateMutation builder context handlers) state mutations

reinstallableCandidates ::
  (QueryDep -> M (Maybe (DepMutation a))) ->
  Query ->
  M [DepMutation a]
reinstallableCandidates candidates (Query query) =
  catMaybes <$> traverse reinstallableOnly (toList query)
  where
    reinstallableOnly dep
      | isNonReinstallableDep dep.package
      = pure Nothing
      | otherwise
      = candidates dep

processQuery ::
  Pretty a =>
  (QueryDep -> M (Maybe (DepMutation a))) ->
  MutationHandlers a s ->
  BuildConfig ->
  StageContext ->
  s ->
  M (StageState a s)
processQuery candidates handlers conf StageContext {env, builder, state, query = query} ext = do
  mutations <- reinstallableCandidates candidates query
  convergeMutations handlers conf builder env stageState mutations
  where
    stageState = initStageState state ext
