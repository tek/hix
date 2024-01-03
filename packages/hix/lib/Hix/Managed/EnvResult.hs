module Hix.Managed.EnvResult where

import Data.List.Extra (nubSortOn)
import Data.These (These (That, These, This))
import Data.These.Combinators (justHere, justThere)
import GHC.Generics (Generically (Generically))

import Hix.Class.Map (nElems, nZipWithKey)
import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Data.Diff (
  BoundsChange,
  BoundsDiffDetail (BoundsDiffDetail),
  Change (Changed, Unchanged),
  Diff (DiffAdded, DiffChanged),
  VersionChange,
  )
import Hix.Managed.Data.EnvResult (EnvResult (..))
import qualified Hix.Managed.Data.EnvState
import Hix.Managed.Data.Mutable (MutableDep, MutableDeps)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import Hix.Managed.Data.Mutation (FailedMutation)
import Hix.Managed.Data.StageResult (stageFailures)
import Hix.Managed.Diff (diffOriginal, reifyBoundsChange, reifyVersionChange)
import Hix.These (maybeThese)

data DepModification =
  DepAdded
  |
  DepUpdated (These Version (These (Maybe Version) (Maybe Version)))
  deriving stock (Eq, Show, Generic)

data DepResultDetail =
  DepModified DepModification
  |
  DepUnmodified
  deriving stock (Eq, Show, Generic)

data DepResult =
  DepResult {
    package :: MutableDep,
    version :: Version,
    bounds :: VersionBounds,
    detail :: DepResultDetail
  }
  deriving stock (Eq, Show, Generic)

depResultId :: DepResult -> MutableId
depResultId DepResult {package, version} =
  MutableId {name = package, version}

depResult :: MutableDep -> VersionChange -> BoundsChange -> Maybe DepResult
depResult package versionChange boundsChange = do
  version <- reifyVersionChange versionChange
  pure DepResult {
    package,
    version,
    bounds = reifyBoundsChange boundsChange,
    detail
  }
  where
    detail = case versionChange of
      Changed (DiffAdded _) -> DepModified DepAdded
      Changed (DiffChanged original _ _) ->
        DepModified (DepUpdated (maybe This (flip These) boundsUpdate original))
      Unchanged _
        | Just b <- boundsUpdate
        -> DepModified (DepUpdated (That b))
      _ -> DepUnmodified


    boundsUpdate = case boundsChange of
      Changed (DiffChanged _ _ (BoundsDiffDetail det)) ->
        maybeThese (diffOriginal <$> justHere det) (diffOriginal <$> justThere det)
      _ -> Nothing

deps :: EnvResult -> [DepResult]
deps EnvResult {state = Nothing} =
  []
deps EnvResult {state = Just state} =
  catMaybes (nElems dv)
  where
    dv :: MutableDeps (Maybe DepResult)
    dv = nZipWithKey depResult state.versions state.bounds

data DepResults =
  DepResults {
    added :: [DepResult],
    updated :: [DepResult],
    unmodified :: [DepResult]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically DepResults)

-- TODO When this is used after merging envs for a report, it should probably combine packages from different lists.
-- If one env updates a package, we don't want to report it as unmodified because there's another env with the same dep.
-- Most likely the output should be env-keyed though, but not sure this is always desirable.
normalizeDepResults :: DepResults -> DepResults
normalizeDepResults DepResults {..} =
  DepResults {added = normalize added, updated = normalize updated, unmodified = normalize unmodified}
  where
    normalize = nubSortOn (.package)

grouped :: EnvResult -> DepResults
grouped result =
  normalizeDepResults DepResults {..}
  where
    (added, updated, unmodified) = foldr (flip step) mempty (deps result)

    step (a, up, un) dep =
      case dep.detail of
        DepModified DepAdded -> (dep : a, up, un)
        DepModified DepUpdated {} -> (a, dep : up, un)
        DepUnmodified -> (a, up, dep : un)

failures :: EnvResult -> [FailedMutation]
failures EnvResult {summaries} =
  stageFailures =<< toList summaries
