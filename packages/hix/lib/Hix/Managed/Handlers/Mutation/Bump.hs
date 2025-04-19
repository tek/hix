module Hix.Managed.Handlers.Mutation.Bump where

import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.Version (Version)
import qualified Hix.Data.VersionBounds as VersionBounds
import Hix.Data.VersionBounds (VersionBounds, fromLower)
import Hix.Managed.Build.Mutation (buildCandidate)
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import qualified Hix.Managed.Data.Bump
import Hix.Managed.Data.Bump (Bump (Bump))
import qualified Hix.Managed.Data.Constraints
import Hix.Managed.Data.Constraints (MutationConstraints (MutationConstraints))
import Hix.Managed.Data.Initial (Initial, initial)
import Hix.Managed.Data.MutableId (MutableId)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (
  BuildMutation,
  DepMutation (DepMutation),
  MutationResult (MutationFailed, MutationSuccess),
  )
import Hix.Managed.Data.MutationState (MutationState)
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers (MutationHandlers))
import Hix.Version (nextMajor)

updateConstraintsBump :: MutableId -> PackageId -> MutationConstraints -> MutationConstraints
updateConstraintsBump _ PackageId {version} MutationConstraints {..} =
  MutationConstraints {mutation = fromLower version, ..}

updateBound :: Version -> VersionBounds -> VersionBounds
updateBound = VersionBounds.withUpper . nextMajor

-- TODO Avoid building unchanged candidates after the first build of the same set of deps.
processMutationBump ::
  Initial SolverState ->
  DepMutation Bump ->
  (BuildMutation -> M (Maybe MutationState)) ->
  M MutationResult
processMutationBump solverState DepMutation {package, mutation = Bump {version, changed}} build =
  builder version <&> \case
    Just (candidate, solverResult, state) ->
      MutationSuccess {candidate, changed, state, solverState = solverResult}
    Nothing ->
      MutationFailed
  where
    builder = buildCandidate build updateBound updateConstraintsBump (initial solverState) package

handlersBump :: MutationHandlers Bump
handlersBump = MutationHandlers {process = processMutationBump}
