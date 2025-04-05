module Hix.Managed.Data.EnvState where

import Distribution.Pretty (Pretty (pretty))
import GHC.Generics (Generically (Generically))
import Text.PrettyPrint (hang, ($+$))

import Hix.Data.Overrides (Overrides)
import Hix.Managed.Data.Diff (BoundsChange, VersionChange)
import Hix.Managed.Data.Mutable (MutableDeps)

-- | All the @*Diff@ fields are initialized as @DiffAbsent@ if there is no entry in the managed state, using the flake
-- config's packages to determine the set of dependencies.
data EnvState =
  EnvState {
    -- | Bounds can differ from the lowest or highest version that was tested.
    -- As of now, this only matters for Bump, which uses the next-greater major version as upper bound.
    bounds :: MutableDeps BoundsChange,

    -- | The precise version determined to be working for the env's target bound.
    versions :: MutableDeps VersionChange,

    -- | Copied from 'versions' at the end of an env flow whenever the final value for a dep is @DiffAdded@.
    initial :: MutableDeps VersionChange,

    -- | Overrides from the persisted state may be empty initially.
    overrides :: Overrides,

    -- | Solver overrides from the persisted state may be empty initially.
    solver :: Overrides
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via (Generically EnvState)

instance Pretty EnvState where
  pretty EnvState {..} =
    hang "bounds:" 2 (pretty bounds) $+$
    hang "versions:" 2 (pretty versions) $+$
    hang "initial:" 2 (pretty initial) $+$
    hang "overrides:" 2 (pretty overrides) $+$
    hang "solver:" 2 (pretty solver)
