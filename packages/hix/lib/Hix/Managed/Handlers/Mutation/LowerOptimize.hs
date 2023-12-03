module Hix.Managed.Handlers.Mutation.LowerOptimize where

import Control.Monad (foldM)
import Data.Foldable.Extra (firstJustM)
import qualified Data.List.NonEmpty.Extra as NonEmpty
import Exon (exon)
import Path (Abs, Dir, Path)

import Hix.Data.Bounds (TargetBound (TargetLower))
import Hix.Data.EnvName (EnvName)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major), NewRange (NewRange), NewVersion (NewVersion))
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (
  BuildMutation,
  DepMutation (DepMutation),
  MutationResult (MutationKeep, MutationSuccess),
  )
import Hix.Managed.Build.Solve (buildWithSolver)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers (MutationHandlers))
import Hix.Managed.Handlers.Solve (SolveHandlers)
import qualified Hix.Managed.Lower.Data.LowerOptimize
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize (LowerOptimize), LowerOptimizeState (LowerOptimizeState))
import Hix.Pretty (showP)
import Hix.Version (setLowerBound)

processMutationLowerOptimize ::
  SolveHandlers ->
  HackageHandlers ->
  LowerOptimizeState ->
  DepMutation LowerOptimize ->
  (BuildMutation -> M (Maybe ManagedState)) ->
  M (MutationResult LowerOptimizeState)
processMutationLowerOptimize solve hackage state mutation build = do
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
      buildWithSolver solve hackage build TargetLower state.solverBounds candidate

    DepMutation {package, mutation = LowerOptimize {majors, range}} = mutation

handlersLowerOptimize ::
  HackageHandlers ->
  (Path Abs Dir -> EnvName -> M SolveHandlers) ->
  Path Abs Dir ->
  EnvName ->
  M (MutationHandlers LowerOptimize LowerOptimizeState)
handlersLowerOptimize hackage mkSolve root env = do
  solve <- mkSolve root env
  pure MutationHandlers {
    process = processMutationLowerOptimize solve hackage
  }
