module Hix.Managed.Lower.Init where

import Exon (exon)

import Hix.Class.Map (nCatMaybes, nKeysSet)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (fromUpper)
import Hix.Managed.Build (processQuery)
import Hix.Managed.Cabal.Data.SolverState (solverState)
import Hix.Managed.Constraints (fromVersions)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.Constraints
import Hix.Managed.Data.Constraints (MutationConstraints (MutationConstraints))
import qualified Hix.Managed.Data.EnvContext
import qualified Hix.Managed.Data.LowerConfig
import Hix.Managed.Data.LowerConfig (LowerConfig)
import Hix.Managed.Data.Mutable (MutableDep, MutableDeps)
import Hix.Managed.Data.MutableId (MutableId)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectResult (ProjectResult)
import qualified Hix.Managed.Data.StageContext
import Hix.Managed.Data.StageContext (StageContext)
import Hix.Managed.Data.StageResult (StageResult)
import Hix.Managed.Data.StageState (BuildStatus, BuildSuccess)
import Hix.Managed.Flow (Flow, execStage)
import qualified Hix.Managed.Handlers.Build
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers)
import qualified Hix.Managed.Handlers.Mutation.Lower as Mutation
import Hix.Managed.Lower.Candidates (candidatesInit)
import Hix.Managed.Lower.Data.LowerMode (lowerInitMode)
import Hix.Managed.Process (processProject)
import Hix.Managed.Report (describeIterations)
import Hix.Managed.StageResult (stageResult)

lowerInitUpdate :: Bool -> MutableId -> PackageId -> MutationConstraints -> MutationConstraints
lowerInitUpdate _ _ PackageId {version} MutationConstraints {..} =
  MutationConstraints {mutation = fromUpper version, ..}

success :: Map MutableDep BuildSuccess -> Natural -> Text
success _ iterations =
  [exon|Found initial lower bounds for all deps after #{iter}.|]
  where
    iter = describeIterations iterations

failure :: Natural -> Text
failure iterations =
  [exon|Couldn't find working initial lower bounds for some deps after #{describeIterations iterations}.|]

-- TODO This could instead only update the initial versions, so that a subsequent @lower.optimize@ can skip the versions
-- it elaborated before.
-- But it would require some nontrivial changes to the state update mechanisms.
--
-- | Determine initial lower bounds for the selected dependencies if they have none or @--reset@ was specified.
-- If none of the selected deps need to be processed, the state is not updated.
-- Otherwise, the lower bounds of _all_ deps are reset to the initial ones, requiring the user to run @lower.optimize@
-- again.
lowerInit ::
  LowerHandlers ->
  LowerConfig ->
  BuildConfig ->
  StageContext ->
  M StageResult
lowerInit handlers conf buildConf context =
  stageResult success failure <$> processQuery handlers.build.hackage candidates mutationHandlers buildConf context ext
  where
    candidates = candidatesInit handlers.versions (nKeysSet (nCatMaybes keep :: MutableDeps Version))

    mutationHandlers = Mutation.handlersLower buildConf lowerInitMode lowerInitUpdate

    ext = solverState context.env.solverBounds context.env.deps (fromVersions fromUpper keep)

    keep | conf.reset = mempty
         | otherwise = context.initial

lowerInitStage ::
  LowerHandlers ->
  LowerConfig ->
  BuildConfig ->
  Flow BuildStatus
lowerInitStage handlers conf buildConf =
  execStage (lowerInit handlers conf buildConf)

lowerInitMain ::
  LowerConfig ->
  LowerHandlers ->
  ProjectContext ->
  M ProjectResult
lowerInitMain conf handlers project =
  processProject handlers.build project (void (lowerInitStage handlers conf project.build))
