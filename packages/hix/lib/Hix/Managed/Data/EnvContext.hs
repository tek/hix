module Hix.Managed.Data.EnvContext where

import Distribution.Pretty (Pretty (..))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Bounds (Ranges)
import Hix.Data.EnvName (EnvName)
import Hix.Data.VersionBounds (Bound)
import Hix.Managed.Cabal.Data.Config (GhcDb)
import Hix.Managed.Data.Mutable (MutableDep)
import Hix.Managed.Data.Targets (Targets)
import Hix.Pretty (hpretty)

newtype EnvDeps =
  EnvDeps { mutable :: Set MutableDep }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

instance Pretty EnvDeps where
  pretty EnvDeps {mutable} = hpretty mutable

-- | Static data defining a managed bounds job for an environment.
data EnvContext =
  EnvContext {
    -- | The name of the Hix environment used to build this job.
    env :: EnvName,

    -- | The bound managed by this environment.
    bound :: Bound,

    -- | Override for the package database, which is usually obtained by context query.
    -- In tests, this can be used to supply an in-memory set of packages.
    --
    -- The Nix package DB consists of a GHC directory in the store created by @ghcWithPackages@.
    -- Cabal interacts with this DB by executing the wrapper scripts for @ghc-pkg@ and @ghc@, which are hardwired to use
    -- the directory @package.conf.d@ in the same store path.
    ghc :: Maybe GhcDb,

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

instance Pretty EnvContext where
  pretty EnvContext {env, bound} = pretty env <+> brackets (pretty bound)
