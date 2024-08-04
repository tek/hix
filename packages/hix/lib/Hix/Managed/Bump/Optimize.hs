module Hix.Managed.Bump.Optimize where

import Exon (exon)

import Hix.Class.Map (nMap, nTransformMaybe, (!?))
import Hix.Data.Bounds (Ranges)
import Hix.Data.Monad (M)
import qualified Hix.Data.Overrides
import Hix.Data.VersionBounds (fromLower, majorRange)
import Hix.Managed.Build (processQuery)
import Hix.Managed.Bump.Candidates (candidatesBump)
import qualified Hix.Managed.Cabal.Data.SolverState
import Hix.Managed.Cabal.Data.SolverState (SolverFlags (SolverFlags), solverState)
import Hix.Managed.Constraints (fromVersions, preferInstalled, preferRanges, preferVersions)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.Bump (Bump (..))
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvDeps)
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (MutableBounds, MutableDep, MutableVersions, depName)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import Hix.Managed.Data.Mutation (DepMutation (..))
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Data.Query (Query (Query))
import qualified Hix.Managed.Data.StageContext
import Hix.Managed.Data.StageContext (StageContext (StageContext))
import Hix.Managed.Data.StageResult (StageResult, StageSummary (StageNoAction, StageReport))
import Hix.Managed.Data.StageState (BuildStatus, BuildSuccess)
import Hix.Managed.Flow (Flow, execStage, execStatelessStage)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Cabal (CabalHandlers, installedVersions)
import Hix.Managed.Handlers.Mutation.Bump (handlersBump)
import Hix.Managed.Process (processProject)
import Hix.Managed.Report (describeIterations)
import Hix.Managed.StageResult (stageResult)

-- TODO this is still janky because it uses reified bound changes (@initialBounds@), which means a loss of precision.
-- If the user specified <=1.2.3, we end up with <1.2.3.
-- Better to drag the actual ranges through from the start.
stateBoundsForNonInstalled :: MutableBounds -> MutableVersions -> Ranges
stateBoundsForNonInstalled initialBounds =
  nTransformMaybe \ package -> \case
    Nothing -> do
      bounds <- initialBounds !? package
      pure (depName package, majorRange bounds)
    Just _ -> Nothing

-- | Solver params for Bump consist of, in decreasing order of precedence:
--
-- - User-specified bounds (added by the constructor 'solverState').
--
-- - The existing overrides from previous runs, used as preferred versions.
--   This ensures that the solver selects the same versions as in the last run while still allowing newer versions for
--   transitive dependencies that are later in the candidate list.
--
-- - Installed versions from the package db, used as lower bounds.
--
-- - Initial bounds from the state (and potentially from the user config if @--read-upper-bounds@ was used) for packages
--   without an installed version.
--   This applies to dependencies on local packages that have not been published to central Hackage (but have versions
--   available on a custom Hackage) and pulled into the project's nixpkgs, and which are targets of a separate managed
--   env (which means that they are not included in the env's GHC package set as local derivations like targets are).
--   If this was the first run, so that no previously determined overrides are present in the state, then these deps
--   would not have installed versions that could be preferred for stability.
--   While the user could specify solver bounds to mitigate this, it's likely that an existing regular bound will be the
--   safest bet.
--   Otherwise the solver would choose the latest version from the custom Hackage, which may mean breakage.
--
-- - The preference for using the installed version for mutable deps, which is a fallback for packages without overrides
--   (since their chosen versions are installed).
bumpSolverParams ::
  EnvDeps ->
  CabalHandlers ->
  Initial MutationState ->
  MutableBounds ->
  EnvConstraints
bumpSolverParams deps cabal (Initial state) initialBounds =
  preferVersions (nMap (.version) state.overrides) <>
  fromVersions fromLower installed <>
  preferRanges (stateBoundsForNonInstalled initialBounds installed) <>
  preferInstalled deps.mutable
  where
    installed = installedVersions cabal deps.mutable

success :: Map MutableDep BuildSuccess -> Natural -> Text
success _ iterations =
  [exon|Found working latest versions for all deps after #{iter}.|]
  where
    iter = describeIterations iterations

failure :: Natural -> Text
failure iterations =
  [exon|Couldn't find working latest versions for some deps after #{describeIterations iterations}.|]

bumpBuild ::
  BuildHandlers ->
  BuildConfig ->
  StageContext ->
  M StageResult
bumpBuild handlers conf stage@StageContext {env, builder, state, initialBounds} = do
  result <- processQuery (candidatesBump handlers) handlersBump conf stage ext
  pure (stageResult success failure result)
  where
    ext = solverState env.solverBounds env.deps (bumpSolverParams env.deps builder.cabal state initialBounds) flags
    flags = SolverFlags {allowNewer = True, forceRevisions = False}

bumpBuildStage ::
  BuildHandlers ->
  BuildConfig ->
  Flow BuildStatus
bumpBuildStage handlers conf =
  execStage "bump" (bumpBuild handlers conf)

bumpReportStage ::
  BuildHandlers ->
  Flow BuildStatus
bumpReportStage handlers =
  execStatelessStage "bump-report" \ StageContext {query = Query query} ->
    toSummary . toCandidates <$> traverse (candidatesBump handlers) (toList query)
  where
    toSummary = \case
      Just cs -> StageReport "Found new versions:" cs
      Nothing -> StageNoAction (Just "All dependencies are up to date.")

    toCandidates = nonEmpty . fmap toCandidate . catMaybes

    toCandidate DepMutation {package = name, mutation = Bump {version}} =
      MutableId {..}

bumpStages ::
  BuildHandlers ->
  BuildConfig ->
  Flow BuildStatus
bumpStages handlers conf
  | conf.lookup
  = bumpReportStage handlers
  | otherwise
  = bumpBuildStage handlers conf

bumpOptimizeMain ::
  BuildHandlers ->
  ProjectContext ->
  M ProjectResult
bumpOptimizeMain handlers project =
  processProject handlers project (void (bumpStages handlers project.build))
