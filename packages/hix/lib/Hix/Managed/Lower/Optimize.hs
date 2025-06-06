module Hix.Managed.Lower.Optimize where

import Exon (exon)

import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.VersionBounds (Bound (..), fromUpper)
import Hix.Managed.Build (processQuery)
import Hix.Managed.Cabal.Data.SolverState (solverState)
import Hix.Managed.Constraints (fromVersions, preferInstalledUnlessBounded)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.Constraints
import Hix.Managed.Data.Constraints (MutationConstraints (MutationConstraints))
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (MutableDep)
import Hix.Managed.Data.MutableId (MutableId)
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState (MutationState))
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectResult (ProjectResult)
import qualified Hix.Managed.Data.StageContext
import Hix.Managed.Data.StageContext (StageContext (StageContext))
import Hix.Managed.Data.StageResult (StageResult)
import Hix.Managed.Data.StageState (BuildSuccess)
import Hix.Managed.Flow (Flow, runStage_)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Mutation.Lower as Mutation
import Hix.Managed.Lower.Candidates (candidatesOptimize)
import Hix.Managed.Lower.Data.LowerMode (lowerOptimizeMode)
import Hix.Managed.Process (processProjectSimple)
import Hix.Managed.Report (describeIterations)
import Hix.Managed.StageResult (stageResult)

lowerOptimizeUpdate :: Bool -> MutableId -> PackageId -> MutationConstraints -> MutationConstraints
lowerOptimizeUpdate _ _ PackageId {version} MutationConstraints {..} =
  MutationConstraints {mutation = fromUpper version, ..}

success :: Map MutableDep BuildSuccess -> Word -> Text
success _ iterations =
  [exon|Found optimal lower bounds for all deps after #{describeIterations iterations}.|]

failure :: Word -> Text
failure iterations =
  [exon|Couldn't find working lower bounds for some deps after #{describeIterations iterations}.|]

lowerOptimize ::
  BuildHandlers ->
  BuildConfig ->
  StageContext ->
  M StageResult
lowerOptimize handlers conf context@StageContext {env, state = Initial MutationState {versions}} =
  stageResult success failure <$> processQuery candidates mutationHandlers conf context ext
  where
    candidates = candidatesOptimize handlers.versions context.initialVersions

    mutationHandlers = Mutation.handlersLower conf lowerOptimizeMode lowerOptimizeUpdate

    ext = solverState env.solverBounds env.deps solverParams def

    solverParams = preferInstalledUnlessBounded BoundUpper (fromVersions fromUpper versions)

lowerOptimizeStage ::
  BuildHandlers ->
  BuildConfig ->
  Flow ()
lowerOptimizeStage handlers conf =
  runStage_ "optimize" (lowerOptimize handlers conf)

lowerOptimizeMain :: BuildHandlers -> ProjectContext -> M ProjectResult
lowerOptimizeMain = processProjectSimple lowerOptimizeStage
