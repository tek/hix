module Hix.Managed.QueryDep where

import Hix.Class.Map ((!!))
import qualified Hix.Data.Overrides
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Data.Initial (Initial (Initial))
import Hix.Managed.Data.Mutable (MutableBounds, MutableDep, depName)
import qualified Hix.Managed.Data.MutationState
import Hix.Managed.Data.MutationState (MutationState)
import qualified Hix.Managed.Data.QueryDep
import Hix.Managed.Data.QueryDep (QueryDep (QueryDep))
import qualified Hix.Managed.Handlers.Cabal
import Hix.Managed.Handlers.Cabal (CabalHandlers)

-- | Add the current bounds to the queried deps.
-- This is used when determining candidates.
queryDep ::
  CabalHandlers ->
  Initial MutationState ->
  MutableBounds ->
  MutableDep ->
  QueryDep
queryDep cabal (Initial state) bounds package =
  QueryDep {
    package,
    installed = cabal.installedVersion name,
    current = join (state.versions !! package),
    override = (.version) <$> state.overrides !! name,
    bounds = fold (bounds !! package)
  }
  where
    name = depName package

simpleQueryDep :: MutableDep -> VersionBounds -> QueryDep
simpleQueryDep package bounds =
  QueryDep {installed = Nothing, current = Nothing, override = Nothing, ..}
