module Hix.Managed.Handlers.Mutation.LowerInit where

import Data.Foldable.Extra (firstJustM)
import Exon (exon)
import Path

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
  MutationResult (MutationFailed, MutationSuccess),
  )
import Hix.Managed.Build.Solve (buildWithSolver)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers (MutationHandlers))
import Hix.Managed.Handlers.Solve (SolveHandlers)
import qualified Hix.Managed.Lower.Data.LowerInit
import Hix.Managed.Lower.Data.LowerInit (LowerInit (LowerInit), LowerInitState (..))
import Hix.Pretty (showP)
import Hix.Version (setLowerBound)

processMutationLowerInit ::
  SolveHandlers ->
  HackageHandlers ->
  LowerInitState ->
  DepMutation LowerInit ->
  (BuildMutation -> M (Maybe ManagedState)) ->
  M (MutationResult LowerInitState)
processMutationLowerInit solve hackage state mutation build = do
  firstJustM buildMajor majors <&> \case
    Just (candidate, newManaged, newBounds) ->
      MutationSuccess candidate newManaged LowerInitState {solverBounds = newBounds}
    Nothing ->
      MutationFailed
  where
    buildMajor Major {prefix, versions} = do
      Log.debug [exon|Building major #{showP prefix} for '##{package}'|]
      firstJustM buildCandidate versions

    buildCandidate version = do
      let candidate = Candidate {
        version = NewVersion {package, version},
        range = NewRange (setLowerBound version range)
      }
      buildWithSolver solve hackage build TargetLower state.solverBounds candidate

    DepMutation {package, mutation = LowerInit {majors, range}} = mutation

handlersLowerInit ::
  HackageHandlers ->
  (Path Abs Dir -> EnvName -> M SolveHandlers) ->
  Path Abs Dir ->
  EnvName ->
  M (MutationHandlers LowerInit LowerInitState)
handlersLowerInit hackage mkSolve root env = do
  solve <- mkSolve root env
  pure MutationHandlers {
    process = processMutationLowerInit solve hackage
  }
