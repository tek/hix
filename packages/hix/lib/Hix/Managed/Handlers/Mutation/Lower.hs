module Hix.Managed.Handlers.Mutation.Lower where

import Control.Monad (foldM)
import Data.Foldable.Extra (firstJustM)
import Data.Generics.Labels ()
import Exon (exon)

import Hix.Data.Monad (M)
import Hix.Data.PackageId (PackageId)
import qualified Hix.Data.Version
import Hix.Data.Version (Major (Major))
import Hix.Data.VersionBounds (withLower)
import qualified Hix.Log as Log
import Hix.Managed.Build.Mutation (buildCandidate)
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.Constraints (MutationConstraints)
import qualified Hix.Managed.Data.Lower
import Hix.Managed.Data.Lower (Lower (Lower))
import Hix.Managed.Data.MutableId (MutableId)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (BuildMutation, DepMutation (DepMutation), MutationResult (MutationSuccess))
import Hix.Managed.Data.MutationState (MutationState)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers (MutationHandlers))
import qualified Hix.Managed.Lower.Data.LowerMode
import Hix.Managed.Lower.Data.LowerMode (LowerMode)
import Hix.Pretty (showP)

-- TODO would be nice if the deps that succeed as transitive deps would have their candidates trimmed so that it
-- immediately returns success when the version is encountered that succeeded most recently that way.
processMutationLower ::
  BuildConfig ->
  LowerMode ->
  (Bool -> MutableId -> PackageId -> MutationConstraints -> MutationConstraints) ->
  SolverState ->
  DepMutation Lower ->
  (BuildMutation -> M (Maybe MutationState)) ->
  M (MutationResult SolverState)
processMutationLower conf mode update solver DepMutation {package, retract, mutation = Lower {majors}} build = do
  foldM buildMajor (Right 0, Nothing) majors <&> \case
    (_, Just (candidate, newSolver, newState)) ->
      MutationSuccess candidate newState newSolver
    (_, Nothing) ->
      mode.noSuccess
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

      | mode.firstSuccess
      , Just _ <- prev
      = pure (failed, prev)

      | otherwise
      = do
        Log.debug [exon|Building major #{showP prefix} for '##{package}'|]
        firstJustM builder versions <&> \case
          Just result -> (Left 0, Just result)
          Nothing -> (bimap (+ 1) (+ 1) failed, prev)

    builder = buildCandidate build withLower (update retract) solver package

handlersLower ::
  BuildConfig ->
  LowerMode ->
  (Bool -> MutableId -> PackageId -> MutationConstraints -> MutationConstraints) ->
  MutationHandlers Lower SolverState
handlersLower conf mode updatePackageParams =
  MutationHandlers {process = processMutationLower conf mode updatePackageParams}
