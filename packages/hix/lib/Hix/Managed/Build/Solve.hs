module Hix.Managed.Build.Solve where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.Map.Strict as Map
import Exon (exon)

import Hix.Class.Map (ntFromList, ntMap, (!!))
import Hix.Data.Bounds (BoundExtension (LowerBoundExtension, UpperBoundExtension), BoundExtensions)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (BuildMutation (BuildMutation))
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import Hix.Managed.Data.ManagedOp (ManagedOp (OpBump, OpLowerStabilize))
import qualified Hix.Managed.Data.SolverParams
import Hix.Managed.Data.SolverParams (BoundMutation (ExtendedBound, RetractedBound), SolverParams, updatePackageParams)
import qualified Hix.Managed.Handlers.Solve
import Hix.Managed.Handlers.Solve (SolveHandlers)
import qualified Hix.Managed.Solve.Changes
import Hix.Managed.Solve.Changes (processSolverPlan)
import Hix.Pretty (showP)

-- | Use the version selected by the solver as the candidate's new extension bound for future solver runs.
--
-- TODO this would be easier for stabilize if we'd toposort the dependencies.
-- Does Cabal have an interface for that?
updateSolverParams :: ManagedOp -> Candidate -> PackageId -> SolverParams -> SolverParams
updateSolverParams op Candidate {package = PackageId {name = candidate}} PackageId {name, version} =
  uncurry (updatePackageParams name) newParams
  where
    -- TODO unclear whether only candidates should be retracted.
    -- Assume dep A is first and resolves version V1 for dep C, and then dep B gets version V2 for dep C.
    -- V2 cannot be higher than V1 because otherwise B would have been lower in the plan for A than in its own plan,
    -- which is impossible since we're choosing the first version from the bottom.
    newParams
      | isCandidate
      , OpLowerStabilize <- op
      = (True, RetractedBound version)
      | otherwise
      = (False, ExtendedBound version)

    isCandidate = candidate == name

logStart :: PackageId -> SolverParams -> M ()
logStart version params = do
  Log.debug [exon|Starting solver build for '#{showP version}'|]
  Log.debug [exon|Solver params: #{showP params}|]

directBounds :: ManagedOp -> [PackageId] -> BoundExtensions
directBounds op versions =
  ntFromList (extension <$> versions)
  where
    extension PackageId {name, version} = (name, cons version)
    cons | OpBump <- op = UpperBoundExtension
         | otherwise = LowerBoundExtension

overrideIsLocal :: SolverParams -> PackageId -> Bool
overrideIsLocal params package =
  fromMaybe False (params !! package.name).local

-- | Run the solver with the current bounds for all of the target set's dependencies, write the plan's versions that
-- differ from the _installed_ ones (the package db for the GHC created by Nix) as overrides to the state file, and run
-- the build.
--
-- The new bounds are discarded when the build fails since we're returning 'MaybeT' and the @build@ invocation returns
-- 'Nothing' on failure.
--
-- Otherwise, update the solver bounds with the new versions for _all_ direct dependencies (@projectDeps@)
-- to pin them down for the ultimate result, and to avoid that the solver picks a later version (for @lower@ runs) when
-- examining subsequent mutations (since Cabal may prefer installed or later versions if given the freedeom).
--
-- TODO abort early when the candidate is already in the solver bounds and doesn't match the range.
-- not necessary if we topsort the candidates before running the build
buildWithSolver ::
  SolveHandlers ->
  (BuildMutation -> M (Maybe ManagedState)) ->
  ManagedOp ->
  SolverParams ->
  Candidate ->
  M (Maybe (Candidate, ManagedState, SolverParams))
buildWithSolver solve build op params candidate = do
  logStart candidate.package params
  runMaybeT do
    plan <- MaybeT (solve.solveForVersion op params candidate.package)
    solverChanges <- lift (processSolverPlan allDeps plan)
    newState <- MaybeT (build (mutation solverChanges))
    let newParams = foldr (updateSolverParams op candidate) params solverChanges.projectDeps
    lift (Log.debug [exon|New solver params: #{showP newParams}|])
    pure (candidate, newState, newParams)
  where
    allDeps = Map.keysSet (ntMap params)

    mutation changes =
      BuildMutation {
        candidate,
        newVersions = filter (not . overrideIsLocal params) changes.overrides,
        accOverrides = [],
        newBounds = directBounds op changes.projectDeps
      }
