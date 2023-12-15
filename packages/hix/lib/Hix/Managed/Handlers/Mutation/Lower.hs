module Hix.Managed.Handlers.Mutation.Lower where

import Control.Monad (foldM)
import Data.Foldable.Extra (firstJustM)
import Data.Generics.Labels ()
import Exon (exon)

import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerConfig)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major), NewRange (NewRange))
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (BuildMutation, DepMutation (DepMutation), MutationResult (MutationSuccess))
import Hix.Managed.Build.Solve (buildWithSolver)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import Hix.Managed.Data.ManagedConfig (ManagedOp)
import Hix.Managed.Handlers.Build (EnvBuilder)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers (MutationHandlers))
import Hix.Managed.Handlers.Solve (SolveHandlers)
import qualified Hix.Managed.Lower.Data.Lower
import Hix.Managed.Lower.Data.Lower (Lower (Lower), LowerState (LowerState))
import Hix.Pretty (showP)
import Hix.Version (setLowerBound)

-- TODO would be nice if the deps that succeed as transitive deps would have their candidates trimmed so that it
-- immediately returns success when the version is encountered that succeeded most recently that way.
processMutationLower ::
  SolveHandlers ->
  ManagedOp ->
  LowerConfig ->
  LowerState ->
  DepMutation Lower ->
  (BuildMutation -> M (Maybe ManagedState)) ->
  M (MutationResult LowerState)
processMutationLower solve op conf state mutation build = do
  foldM buildMajor (Right 0, Nothing) majors <&> \case
    (_, Just (candidate, newState, solverParams)) ->
      MutationSuccess candidate newState LowerState {solverParams}
    (_, Nothing) ->
      conf.noSuccess
  where
    -- | We skip all remaining majors when the number of failed majors exceeds the configured limit.
    -- There are different values for failures before the first and after the last success.
    -- This avoid building the project tens of times for versions that cannot succeed.
    --
    -- We also skip all remaining majors when @firstSuccess@ is set, after a major succeeded.
    buildMajor (failed, prev) Major {prefix, versions}
      | Right n <- failed
      , n > conf.maxFailedPre
      = pure (failed, prev)

      | Left n <- failed
      , n > conf.maxFailedPost
      = pure (failed, prev)

      | conf.firstSuccess
      , Just _ <- prev
      = pure (failed, prev)

      | otherwise
      = do
        Log.debug [exon|Building major #{showP prefix} for '##{package}'|]
        firstJustM buildCandidate versions <&> \case
          Just result -> (Left 0, Just result)
          Nothing -> (bimap (+ 1) (+ 1) failed, prev)

    buildCandidate version = do
      let candidate = Candidate {
        package = PackageId {name = package, version},
        range = NewRange (setLowerBound version range)
      }
      buildWithSolver solve build op state.solverParams candidate

    DepMutation {package, mutation = Lower {majors, range}} = mutation

handlersLower ::
  ManagedOp ->
  LowerConfig ->
  (EnvBuilder -> M SolveHandlers) ->
  EnvBuilder ->
  M (MutationHandlers Lower LowerState)
handlersLower op conf mkSolve builder = do
  solve <- mkSolve builder
  pure MutationHandlers {
    process = processMutationLower solve op conf
  }
