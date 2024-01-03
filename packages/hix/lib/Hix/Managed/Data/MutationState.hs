module Hix.Managed.Data.MutationState where

import Data.Map.Merge.Strict (dropMissing, mapMaybeMissing, zipWithMatched)
import Distribution.Pretty (Pretty (pretty))
import GHC.Generics (Generically (Generically))
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.Map (nMerge)
import Hix.Data.Overrides (Overrides)
import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Data.Mutable (MutableBounds, MutableVersions)

data MutationState =
  MutationState {
    bounds :: MutableBounds,
    versions :: MutableVersions,
    overrides :: Overrides,
    initial :: MutableVersions
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically MutationState)

instance Pretty MutationState where
  pretty MutationState {..} =
    hang "bounds:" 2 (pretty bounds) $+$
    hang "versions:" 2 (pretty versions) $+$
    hang "overrides:" 2 (pretty overrides) $+$
    hang "initial:" 2 (pretty initial)

updateBoundsWith :: (Version -> VersionBounds -> VersionBounds) -> MutationState -> MutationState
updateBoundsWith update MutationState {bounds, versions, ..} =
  MutationState {
    bounds = nMerge addBound dropMissing updateBound versions bounds,
    versions,
    ..
  }
  where
    addBound = mapMaybeMissing \ _ -> fmap (flip update mempty)
    updateBound = zipWithMatched \ _ -> maybe id update
