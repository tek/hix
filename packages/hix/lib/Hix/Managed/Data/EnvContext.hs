module Hix.Managed.Data.EnvContext where

import Hix.Data.Bounds (Ranges)
import Hix.Data.EnvName (EnvName)
import Hix.Managed.Cabal.Data.Config (GhcDb)
import Hix.Managed.Data.Mutable (MutableDep)
import Hix.Managed.Data.Targets (Targets)

newtype EnvDeps =
  EnvDeps { mutable :: Set MutableDep }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

-- | Static data defining a managed bounds job for an environment.
data EnvContext =
  EnvContext {
    -- | The name of the Hix environment used to build this job.
    env :: EnvName,

    -- | The package database containing installed packages, corresponding to the set returned by @ghcWithPackages@ in
    -- Nix, with all nonlocal project dependencies.
    -- In production, this points to the dir containing the GHC executables in the Nix store, which Cabal executes to
    -- interact with the database.
    -- In tests, this is a pure set of manually constructed packages.
    ghc :: GhcDb,

    -- | The set of local packages associated with this environment.
    -- Sorted topologically by smart constructor.
    targets :: Targets,

    -- | The names of dependencies associated with this environment.
    deps :: EnvDeps,

    -- | The subset of this environment's remote dependencies whose bounds should be updated.
    query :: NonEmpty MutableDep,

    solverBounds :: Ranges
  }
  deriving stock (Eq, Show, Generic)
