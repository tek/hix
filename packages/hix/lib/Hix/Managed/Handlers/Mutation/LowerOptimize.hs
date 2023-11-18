module Hix.Managed.Handlers.Mutation.LowerOptimize where

import Control.Monad (foldM)
import Data.Foldable.Extra (firstJustM)
import qualified Data.List.NonEmpty.Extra as NonEmpty
import Exon (exon)

import Hix.Data.Bounds (TargetBound (TargetLower))
import Hix.Data.ManagedEnv (ManagedState)
import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major), NewRange (NewRange), NewVersion (NewVersion))
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (
  BuildMutation,
  Candidate (Candidate),
  DepMutation (DepMutation),
  MutationResult (MutationKeep, MutationSuccess),
  )
import Hix.Managed.Build.Solve (buildWithSolver)
import Hix.Managed.Handlers.Build (BuildHandlers)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers (MutationHandlers))
import qualified Hix.Managed.Lower.Data.LowerOptimize
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize (LowerOptimize), LowerOptimizeState (LowerOptimizeState))
import Hix.Monad (M)
import Hix.Pretty (showP)
import Hix.Version (setLowerBound)

processMutationLowerOptimize ::
  BuildHandlers ->
  LowerOptimizeState ->
  DepMutation LowerOptimize ->
  (BuildMutation -> M (Maybe ManagedState)) ->
  M (MutationResult LowerOptimizeState)
processMutationLowerOptimize handlers state mutation build = do
  foldM buildMajor (False, Nothing) (NonEmpty.sortOn (Down . (.prefix)) majors) <&> \case
    (_, Just (candidate, newManaged, newBounds)) ->
      MutationSuccess candidate newManaged LowerOptimizeState {solverBounds = newBounds}
    (_, Nothing) ->
      MutationKeep
  where
    -- | The 'Bool' indicates that we should skip all remaining majors, which is set when a major fails to build and
    -- there has been a previous success, to avoid building the project tens of times for versions that cannot succeed.
    buildMajor (True, prev) _ =
      pure (True, prev)

    buildMajor (False, prev) Major {prefix, versions} = do
      Log.debug [exon|Building major #{showP prefix} for '##{package}'|]
      firstJustM buildCandidate versions <&> \case
        Just result -> (False, Just result)
        Nothing -> (isJust prev, prev)

    buildCandidate version = do
      let candidate = Candidate {
        version = NewVersion {package, version},
        range = NewRange (setLowerBound version range)
      }
      buildWithSolver handlers build TargetLower state.solverBounds candidate

    DepMutation {package, mutation = LowerOptimize {majors, range}} = mutation

handlersLowerOptimize ::
  BuildHandlers ->
  MutationHandlers LowerOptimize LowerOptimizeState
handlersLowerOptimize handlers =
  MutationHandlers {
    process = processMutationLowerOptimize handlers
  }
