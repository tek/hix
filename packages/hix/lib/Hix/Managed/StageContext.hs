module Hix.Managed.StageContext where

import Hix.Class.Map (nMap)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvRequest (EnvRequest (..))
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.EnvState (EnvState (EnvState))
import Hix.Managed.Data.Initial (Initial (Initial))
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState (MutationState))
import Hix.Managed.Data.Query (Query (Query))
import Hix.Managed.Data.StageContext (StageContext (..))
import Hix.Managed.Diff (reifyBoundsChange, reifyVersionChanges)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.QueryDep (queryDep)

-- | Initialize the mutation versions from the persisted state, so the solver bounds are populated by the versions
-- determined in previous runs.
--
-- @bounds@ is initialized empty because it is used only for updating based on candidates, since upper bounds differ
-- from the tested versions.
mutationState :: EnvState -> Initial MutationState
mutationState EnvState {versions, overrides} =
  Initial MutationState {
    bounds = mempty,
    versions = reifyVersionChanges versions,
    overrides
  }

stageContext :: EnvRequest -> EnvState -> StageContext
stageContext EnvRequest {context = env, builder, state = initialState} envState =
  StageContext {
    query = Query (queryDep builder.cabal state initialBounds <$> env.query),
    initial = reifyVersionChanges envState.initial,
    ..
  }
  where
    initialBounds = nMap reifyBoundsChange . (.bounds) <$> initialState
    state = mutationState envState
