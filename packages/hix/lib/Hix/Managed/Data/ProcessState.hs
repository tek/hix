module Hix.Managed.Data.ProcessState where

import Data.Generics.Labels ()
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.Map (nAmend, (!!), nMap)
import Hix.Data.Bounds (Bounds)
import Hix.Data.VersionBounds (majorRange)
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..))
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext)
import Hix.Managed.Data.ProjectState (ProjectState (..))
import Hix.Managed.ManagedPackage (updateRanges, removeLowerBounds)

data ProcessState =
  ProcessState {
    packages :: Packages ManagedPackage,
    state :: ProjectState
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ProcessState where
  pretty ProcessState {..} =
    hang "packages:" 2 (pretty packages)
    $+$
    hang "state:" 2 (pretty state)

updateManagedPackages :: Packages Bounds -> Packages ManagedPackage -> Packages ManagedPackage
updateManagedPackages =
  nAmend \ bounds ->
    updateRanges \ package original ->
      maybe original majorRange (bounds !! package)

-- | 1. Remove lower bounds from the package configs, leave the upper bounds.
--      These are from the flake's unmanaged dependencies, so they should only be used when there is no better
--      information available, i.e. managed dependencies from prior runs.
--   2. Replace bounds in the package configs entirely with managed bounds if they exist.
--      This ensures that Cabal sees managed bounds for local packages instead of stray flake config bounds.
--
-- The package configs are used to populate Cabal's source package DB with fallbacks for local packages in case they
-- don't exist in other DBs.
--
-- TODO Review – this may be obsolete.
initProcessState :: ProjectContext -> ProcessState
initProcessState context =
  ProcessState {
    packages = updateManagedPackages context.state.bounds (nMap removeLowerBounds context.packages),
    state = context.state
  }
