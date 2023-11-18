module Hix.Managed.Handlers.Mutation.Bump where

import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import Hix.Data.Overrides (latestVersionNewer)
import qualified Hix.Data.Version
import Hix.Data.Version (NewRange (NewRange, OldRange), NewVersion (NewVersion))
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (
  BuildMutation (BuildMutation),
  DepMutation (DepMutation),
  MutationResult (MutationFailed, MutationKeep, MutationSuccess, MutationUpdateBounds),
  )
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import qualified Hix.Managed.Handlers.Mutation
import Hix.Managed.Handlers.Mutation (MutationHandlers (MutationHandlers))
import qualified Hix.Managed.Lower.Data.Bump
import Hix.Managed.Lower.Data.Bump (Bump (Bump), BumpState (BumpState))

-- | If the new version isn't newer than the existing override, we don't want to report a bump, but we still want to
-- ensure that the same version builds, since the user might have changed the code in a way that makes it incompatible
-- with the latest version, which may differ from the dev env.
processMutationBump ::
  BumpState ->
  DepMutation Bump ->
  (BuildMutation -> M (Maybe ManagedState)) ->
  M (MutationResult BumpState)
processMutationBump state DepMutation {package, mutation = Bump {version, range}} build =
  build BuildMutation {candidate, newVersions = [], accOverrides = state.overrides, newBounds = []} <&> \case
    Just newManaged
      | latestVersionNewer state.overrides package version ->
        MutationSuccess candidate newManaged BumpState {overrides = newManaged.overrides}
      | otherwise ->
        onlyRange range
    Nothing ->
      MutationFailed
  where
    onlyRange = \case
      NewRange newRange -> MutationUpdateBounds candidate.version newRange
      OldRange -> MutationKeep

    candidate = Candidate {version = NewVersion {package, version}, range}

handlersBump :: MutationHandlers Bump BumpState
handlersBump = MutationHandlers {process = processMutationBump}
