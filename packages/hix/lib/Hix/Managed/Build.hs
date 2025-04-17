module Hix.Managed.Build where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Distribution.Pretty (Pretty)
import Exon (exon)
import Text.PrettyPrint (vcat)

import Hix.Class.Map (nToMaybe)
import qualified Hix.Color as Color
import qualified Hix.Console
import Hix.Console (color, colors)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (IsRevision (..), Override (..), Overrides)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.Version (Version, Versions)
import Hix.Data.VersionBounds (VersionBounds)
import qualified Hix.Log as Log
import Hix.Managed.Build.Solve (solveMutation)
import qualified Hix.Managed.Cabal.Changes
import Hix.Managed.Cabal.Config (isNonReinstallableDep, isReinstallableId)
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.Mutable (MutableDep, addBuildVersions)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (BuildMutation (BuildMutation), DepMutation, MutationResult (..))
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState (MutationState), updateBoundsWith)
import Hix.Managed.Data.NixOutput (PackageDerivation (..))
import Hix.Managed.Data.Query (Query (Query))
import qualified Hix.Managed.Data.QueryDep
import Hix.Managed.Data.QueryDep (QueryDep)
import qualified Hix.Managed.Data.StageContext
import Hix.Managed.Data.StageContext (StageContext (StageContext))
import qualified Hix.Managed.Data.StageState
import Hix.Managed.Data.StageState (
  BuildFailure (PackageFailure, TimeoutFailure),
  BuildResult (BuildFailure, BuildSuccess),
  BuildStatus,
  StageState (failed, iterations),
  buildStatus,
  initStageState,
  justSuccess,
  )
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (EnvBuilder)
import qualified Hix.Managed.Handlers.Cabal
import Hix.Managed.Handlers.Cabal (CabalHandlers (CabalHandlers))
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import Hix.Managed.StageState (updateStageState)
import Hix.Monad (appContext, appContextDebug)
import Hix.Pretty (prettyL, showP, showPL)

logBuildInputs ::
  EnvName ->
  Text ->
  [PackageId] ->
  M ()
logBuildInputs env description overrides = do
  Log.info [exon|Building targets in #{Color.env env} with #{Color.package description}...|]
  Log.debugP (vcat ["Overrides:", prettyL overrides])

logBuildResult :: Text -> BuildResult -> M ()
logBuildResult description result =
  Log.info [exon|Build with ##{Color.package description} #{describeResult result}#{describePackages result}|]
  where
    describeResult = \case
      BuildSuccess _ -> "succeeded"
      BuildFailure (TimeoutFailure _) -> "timed out"
      BuildFailure _ -> "failed"

    describePackages = \case
      BuildFailure (TimeoutFailure pkgs) | not (null pkgs) -> packageFragment pkgs
      BuildFailure (PackageFailure pkgs) -> packageFragment ((.package) <$> toList pkgs)
      _ -> ""

    packageFragment pkgs =
      [exon| in #{names}|]
      where
        names = Text.intercalate ", " (Color.package . (.name) <$> pkgs)

updateMutationState ::
  (Version -> VersionBounds -> VersionBounds) ->
  Versions ->
  Overrides ->
  MutationState ->
  MutationState
updateMutationState updateBound newVersions overrides MutationState {bounds, versions, initial} =
  updateBoundsWith updateBound MutationState {
    bounds,
    versions = addBuildVersions newVersions versions,
    overrides,
    initial
  }

-- | /Note/: This quietly discards non-reinstallable packages.
buildVersions ::
  EnvBuilder ->
  EnvContext ->
  Text ->
  Bool ->
  Versions ->
  [PackageId] ->
  M (Overrides, BuildStatus)
buildVersions builder context description allowRevisions versions overrideVersions = do
  logBuildInputs context.env description reinstallable
  (result, overrides) <- builder.buildTargets allowRevisions versions reinstallable
  logBuildResult description result
  pure (overrides, buildStatus result)
  where
    reinstallable = filter isReinstallableId overrideVersions

buildConstraints ::
  EnvBuilder ->
  EnvContext ->
  Text ->
  Bool ->
  Overrides ->
  SolverState ->
  M (Maybe (Versions, Overrides, BuildStatus))
buildConstraints builder context description allowRevisions prevOverrides state =
  solveMutation builder.cabal context.deps prevRevisions state >>= traverse \ changes -> do
    (overrides, status) <-
      buildVersions builder context description allowRevisions changes.versions changes.overrides
    pure (changes.versions, overrides, status)
  where
    prevRevisions =
      Set.fromList $ nToMaybe prevOverrides \cases
        name Override {version, revision = Just IsRevision} -> Just PackageId {..}
        _ _ -> Nothing

buildMutation ::
  EnvBuilder ->
  EnvContext ->
  MutationState ->
  BuildMutation ->
  M (Maybe MutationState)
buildMutation builder context state BuildMutation {description, solverState, updateBound} =
  result <$> buildConstraints builder context description True state.overrides solverState
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
  MutationSuccess {candidate, changed = True} ->
    Log.verbose [exon|Build succeeded for #{showP candidate}|]
  MutationSuccess {changed = False} ->
    Log.verbose [exon|Build is up to date for '##{package}'|]
  MutationKeep ->
    Log.verbose [exon|No better version found for '##{package}'|]
  MutationFailed ->
    Log.verbose [exon|Could not find a buildable version of #{color colors.blue (showP package)}|]

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
    processReinstallable =
      if isNonReinstallableDep mutation.package
      then pure MutationKeep
      else handlers.process stageState.ext mutation build

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
        let iteration = state.iterations + 1
        newState <- appContextDebug [exon|converging #{Color.env context.env} (iteration #{Color.number iteration})|] do
          build state iteration mutations
        if Map.size newState.success == Map.size state.success
        then pure newState
        -- reversing so the build order is consistent
        else spin newState (reverse newState.failed)

    build statePre iterations mutations = do
      let state = statePre {failed = [], iterations}
      Log.debug [exon|Building targets with mutations: #{showPL mutations}|]
      foldM (validateMutation builder context handlers) state mutations

reinstallableCandidates ::
  (QueryDep -> M (Maybe (DepMutation a))) ->
  Query ->
  M [DepMutation a]
reinstallableCandidates candidates (Query query) =
  appContext "collecting mutation candidates" do
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
  mutations <- postprocess =<< reinstallableCandidates candidates query
  convergeMutations handlers conf builder env stageState mutations
  where
    postprocess | conf.toposortMutations = sortMutations
                | otherwise = pure
    stageState = initStageState state ext
    CabalHandlers {sortMutations} = builder.cabal
