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
  SolverState ->
  DepMutation Bump ->
  (BuildMutation -> M (Maybe MutationState)) ->
  M (MutationResult SolverState)
processMutationBump solver DepMutation {package, mutation = Bump {version, changed}} build =
  builder version <&> \case
    Just (candidate, ext, state) ->
      MutationSuccess {candidate, changed, state, ext}
    Nothing ->
      MutationFailed
  where
    builder = buildCandidate build updateBound updateConstraintsBump solver package

handlersBump :: MutationHandlers Bump SolverState
handlersBump = MutationHandlers {process = processMutationBump}
