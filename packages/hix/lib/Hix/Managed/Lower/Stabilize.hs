module Hix.Managed.Lower.Stabilize where

import Exon (exon)

import Hix.Class.Map (nToMaybe, (!!))
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import qualified Hix.Data.VersionBounds
import Hix.Data.VersionBounds (VersionBounds (VersionBounds), fromLower, fromUpper)
import Hix.Managed.Build (processQuery)
import Hix.Managed.Build.Single (buildVersions)
import Hix.Managed.Cabal.Data.SolverState (solverState)
import Hix.Managed.Constraints (fromVersions)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.Constraints
import Hix.Managed.Data.Constraints (MutationConstraints (MutationConstraints))
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (MutableDep, MutableVersions, depName)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId)
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState (MutationState))
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectResult (ProjectResult)
import qualified Hix.Managed.Data.QueryDep
import qualified Hix.Managed.Data.StageContext
import Hix.Managed.Data.StageContext (StageContext (StageContext))
import Hix.Managed.Data.StageResult (
  StageFailure (FailedPrecondition),
  StageResult,
  StageSummary (StageFailure, StageNoAction),
  )
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success), BuildSuccess)
import Hix.Managed.Flow (Flow, execStatelessStage, runStage_)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers)
import qualified Hix.Managed.Handlers.Mutation.Lower as Mutation
import Hix.Managed.Lower.Candidates (candidatesStabilize)
import Hix.Managed.Lower.Data.LowerMode (lowerStabilizeMode)
import Hix.Managed.Process (processProject)
import Hix.Managed.Report (describeIterations)
import Hix.Managed.StageResult (stageResult)

-- TODO When some initial versions are missing, e.g. because the user added deps and the project broke at the same time,
-- it is up to luck that the solver chooses working versions.
-- Unlikely that there's a useful heuristic though.
initialVersions ::
  MutableVersions ->
  [PackageId]
initialVersions state =
  nToMaybe state \ name -> fmap (initVersion name)
  where
    initVersion name version = PackageId {name = depName name, ..}

buildLowerInit ::
  HackageHandlers ->
  Flow BuildStatus
buildLowerInit hackage = do
  execStatelessStage \ StageContext {env, initial, builder} ->
    buildVersions hackage builder env "initial lower bounds" initial <&> \case
      Success -> StageNoAction (Just "Env builds successfully with the initial bounds.")
      Failure ->
        StageFailure (FailedPrecondition msg)
  where
    msg =
      [
        "Cannot stabilize since the build with initial bounds failed.",
        "Please run 'lower.init --reset' or fix the build manually."
      ]

lowerStabilizeUpdate :: Bool -> MutableId -> PackageId -> MutationConstraints -> MutationConstraints
lowerStabilizeUpdate retract candidate PackageId {name, version} MutationConstraints {..}
  | retract
  , depName candidate.name == name
  = MutationConstraints {mutation = fromLower version, oldest = Just True, ..}
  | VersionBounds {lower = Just _} <- mutation
  = MutationConstraints {mutation = fromLower version, ..}
  | otherwise
  = MutationConstraints {mutation = fromUpper version, ..}

success :: Map MutableDep BuildSuccess -> Natural -> Text
success _ iterations =
  [exon|Found stable lower bounds for all deps after #{iter}.|]
  where
    iter = describeIterations iterations

failure :: Natural -> Text
failure iterations =
  [exon|Couldn't find working lower bounds for some deps after #{describeIterations iterations}.|]

-- | This uses 'lowerInit' for the initial solver bounds, which gets translated to an extended bound treated as an
-- upper.
-- When a stable version was found, it will be set as a retracted bound, treated as a lower.
lowerStabilize ::
  LowerHandlers ->
  BuildConfig ->
  StageContext ->
  M StageResult
lowerStabilize handlers conf context =
  stageResult success failure <$> processQuery handlers.build.hackage candidates mutationHandlers conf context ext
  where
    candidates query = candidatesStabilize handlers.versions query (join (context.initial !! query.package))

    mutationHandlers = Mutation.handlersLower conf lowerStabilizeMode lowerStabilizeUpdate

    ext = solverState context.env.solverBounds context.env.deps (fromVersions fromUpper context.initial)

stabilizeStage ::
  LowerHandlers ->
  BuildConfig ->
  Flow ()
stabilizeStage handlers conf =
  runStage_ (lowerStabilize handlers conf)

stabilizeIfPossible ::
  LowerHandlers ->
  BuildConfig ->
  Flow ()
stabilizeIfPossible handlers conf =
  buildLowerInit handlers.build.hackage >>= \case
    Success -> stabilizeStage handlers conf
    Failure -> unit

validateCurrent ::
  HackageHandlers ->
  Flow BuildStatus
validateCurrent hackage =
  execStatelessStage \ StageContext {env, state = Initial MutationState {versions}, builder} ->
    buildVersions hackage builder env "current lower bounds" versions <&> \case
      Success -> StageNoAction (Just "Env builds successfully with the current bounds.")
      Failure -> StageFailure (FailedPrecondition ["Env does not build successfully with the current bounds."])

lowerStabilizeStages ::
  LowerHandlers ->
  BuildConfig ->
  Flow ()
lowerStabilizeStages handlers conf =
  validateCurrent handlers.build.hackage >>= \case
    Success -> unit
    Failure -> stabilizeIfPossible handlers conf

lowerStabilizeMain ::
  LowerHandlers ->
  ProjectContext ->
  M ProjectResult
lowerStabilizeMain handlers project =
  processProject handlers.build project (lowerStabilizeStages handlers project.build)
