module Hix.Managed.Build.Solve where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.Map.Strict as Map
import Exon (exon)

import Hix.Class.Map (ntFromList, ntInsert, ntMap, (!!))
import Hix.Data.Bounds (
  BoundExtension (LowerBoundExtension, UpperBoundExtension),
  BoundExtensions,
  TargetBound (TargetUpper),
  )
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (BuildMutation (BuildMutation))
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate)
import Hix.Managed.Data.SolverBounds (SolverBound (..), SolverBounds, solverRanges)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Solve
import qualified Hix.Managed.Solve.Changes
import Hix.Managed.Solve.Changes (processSolverPlan)
import Hix.Pretty (showP)

-- | Use the version selected by the solver as the candidate's new upper bound for future solver runs.
updateSolverBound :: SolverBounds -> NewVersion -> SolverBounds
updateSolverBound oldBounds NewVersion {package, version} =
  ntInsert package newBound oldBounds
  where
    oldBound = oldBounds !! package

    newBound = case oldBound of
      Just (ExtendedBound _) -> ExtendedBound version
      -- TODO intersect with new bound, or set new bound, based on @targetBound@?
      -- Note that @targetBound@ must be inverted here, since this is the extension part.
      Just (SpecifiedBounds spec) -> SpecifiedBounds spec
      Just NoBounds -> ExtendedBound version
      Nothing -> ExtendedBound version

logStart :: NewVersion -> SolverBounds -> M ()
logStart version bounds =
  Log.debug [exon|Starting solver build for '#{showP version}' and old bounds: #{showP bounds}|]

directBounds :: TargetBound -> [NewVersion] -> BoundExtensions
directBounds targetBound versions =
  ntFromList (extension <$> versions)
  where
    extension NewVersion {package, version} = (package, cons version)
    cons | TargetUpper <- targetBound = UpperBoundExtension
         | otherwise = LowerBoundExtension

-- | Run the solver with the current bounds for all of the target set's dependencies, write the plan's versions that
-- differ from the _installed_ ones (the packagedb for the GHC created by Nix) to the state file, and run the build.
--
-- The new bounds are discarded when the build fails since we're returning 'MaybeT' and the @build@ invocation returns
-- 'Nothing' on failure.
--
-- Otherwise, update the solver bounds with the new versions for _all_ direct dependencies (@projectDeps@)
-- to pin them down for the ultimate result, and to avoid that the solver picks a later version (for @lower@ runs) when
-- examining subsequent mutations (since Cabal may prefer installed or later versions if given the freedeom).
buildWithSolver ::
  BuildHandlers ->
  (BuildMutation -> M (Maybe ManagedState)) ->
  TargetBound ->
  SolverBounds ->
  Candidate ->
  M (Maybe (Candidate, ManagedState, SolverBounds))
buildWithSolver handlers build targetBound bounds candidate = do
  logStart candidate.version bounds
  runMaybeT do
    newVersions <- MaybeT (handlers.solve.solveForVersion (solverRanges bounds) candidate.version)
    solverChanges <- lift (processSolverPlan allDeps handlers.hackage newVersions)
    new <- MaybeT (build (mutation solverChanges))
    let newBounds = foldl updateSolverBound bounds solverChanges.projectDeps
    lift (Log.debug [exon|New solver bounds: #{showP newBounds}|])
    pure (candidate, new, newBounds)
  where
    allDeps = Map.keysSet (ntMap bounds)
    mutation changes =
      BuildMutation {
        candidate,
        newVersions = changes.versions,
        accOverrides = [],
        newBounds = directBounds targetBound changes.projectDeps
      }
