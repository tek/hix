module Hix.Managed.Lower.Auto where

import qualified Data.Set as Set

import Hix.Class.Map (nToMaybe, nZipR)
import Hix.Data.Monad (M)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState (EnvState))
import qualified Hix.Managed.Data.LowerConfig
import Hix.Managed.Data.LowerConfig (LowerConfig)
import Hix.Managed.Data.Mutable (MutableVersions)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Data.Query (Query (Query))
import qualified Hix.Managed.Data.QueryDep
import Hix.Managed.Data.StageContext (query)
import qualified Hix.Managed.Data.StageResult
import Hix.Managed.Data.StageResult (StageResult (StageResult), StageSummary (StageNoAction))
import Hix.Managed.Data.StageState (BuildStatus (Failure, Success))
import Hix.Managed.Diff (reifyVersionChanges)
import Hix.Managed.Flow (Flow, runStage_, stageError, stageState)
import qualified Hix.Managed.Handlers.Build
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers)
import Hix.Managed.Lower.Init (lowerInitStage)
import Hix.Managed.Lower.Optimize (lowerOptimize)
import Hix.Managed.Lower.Stabilize (stabilizeIfPossible, stabilizeStage, validateCurrent)
import Hix.Managed.Process (processProject)

suggestStabilize :: Flow ()
suggestStabilize = stageError "Re-run with --stabilize to attempt to fix the bounds."

-- | This skips building with initial bounds because it only runs after LowerInit failed, which means that we have no
-- initial bounds.
stabilizeInitFailure ::
  LowerHandlers ->
  LowerConfig ->
  BuildConfig ->
  Flow ()
stabilizeInitFailure handlers conf buildConf
  | conf.stabilize
  = stabilizeStage handlers buildConf
  | otherwise
  = suggestStabilize

-- TODO needs some more work
pristineBounds :: EnvState -> Query -> Maybe Query
pristineBounds EnvState {versions, initial} (Query query) =
  Query <$> nonEmpty (restrictQuery (nToMaybe pristine nameIfJust))
  where
    nameIfJust name = \case
      Just _ -> Just name
      Nothing -> Nothing

    restrictQuery (Set.fromList -> names) =
      filter (flip Set.member names . (.package)) (toList query)

    pristine :: MutableVersions
    pristine = nZipR justEqual (reifyVersionChanges initial) (reifyVersionChanges versions)

    justEqual (Just (Just ini)) (Just current) | ini == current = Just current
    justEqual _ _ = Nothing

optimizePristineBounds ::
  LowerHandlers ->
  BuildConfig ->
  Flow ()
optimizePristineBounds handlers conf = do
  -- TODO principled approach
  env <- stageState id
  runStage_ \ context ->
    case pristineBounds env context.query of
      Just newQuery -> lowerOptimize handlers conf context {query = newQuery}
      Nothing -> pure StageResult {summary = StageNoAction Nothing, state = Nothing}

postInit ::
  LowerHandlers ->
  LowerConfig ->
  BuildConfig ->
  Flow ()
postInit handlers conf buildConf =
  validateCurrent handlers.build.hackage >>= \case
    Success -> optimizePristineBounds handlers buildConf
    Failure | conf.stabilize -> stabilizeIfPossible handlers buildConf
            | otherwise -> suggestStabilize

lowerAutoStages ::
  LowerHandlers ->
  LowerConfig ->
  BuildConfig ->
  Flow ()
lowerAutoStages handlers conf buildConf =
  lowerInitStage handlers conf buildConf >>= \case
    Success | conf.initOnly -> unit
            | otherwise -> postInit handlers conf buildConf
    Failure -> stabilizeInitFailure handlers conf buildConf

lowerAutoMain ::
  LowerConfig ->
  LowerHandlers ->
  ProjectContext ->
  M ProjectResult
lowerAutoMain conf handlers project =
  processProject handlers.build project (lowerAutoStages handlers conf project.build)
