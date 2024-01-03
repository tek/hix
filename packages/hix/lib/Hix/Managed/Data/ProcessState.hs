module Hix.Managed.Data.ProcessState where

import Control.Lens ((%~))
import Data.Generics.Labels ()
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (removeLowerBound)
import Text.PrettyPrint (hang, ($+$))

import Hix.Class.Map (nAmend, nMap)
import Hix.Data.VersionBounds (majorRange)
import qualified Hix.Managed.Data.ManagedPackage
import Hix.Managed.Data.ManagedPackage (ManagedPackage (ManagedPackage))
import Hix.Managed.Data.Mutable (MutableBounds)
import Hix.Managed.Data.Packages (Packages)
import qualified Hix.Managed.Data.ProjectContext
import Hix.Managed.Data.ProjectContext (ProjectContext)
import qualified Hix.Managed.Data.ProjectState
import Hix.Managed.Data.ProjectState (ProjectState)

data ProcessState =
  ProcessState {
    packages :: Packages ManagedPackage,
    state :: ProjectState
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ProcessState where
  pretty ProcessState {..} =
    hang "packages:" 2 (pretty packages) $+$ hang "state:" 2 (pretty state)

-- TODO Check that this is sound.
-- We simply replace the deps in the packages with the updated bounds.
-- These are used only for the installed-package-db, so the solver can propagate bounds from local deps.
-- However, before conversion from ManagedPackageProto, these deps are completely unrelated, and supposed to be used as
-- additional solver bounds.
-- So we have to extract those into a separate bin at that point, and wipe the bounds when constructing ManagedPackage,
-- and then replacing them by the managed bounds if they already exist.
updateManagedPackages :: Packages MutableBounds -> Packages ManagedPackage -> Packages ManagedPackage
updateManagedPackages =
  nAmend \ bounds ManagedPackage {..} -> ManagedPackage {mutable = nMap majorRange bounds, ..}

removeLowerBounds :: ManagedPackage -> ManagedPackage
removeLowerBounds =
  #mutable %~ nMap removeLowerBound

-- | We remove lower bounds because they are likely to interfere with LowerInit, but upper bounds are useful.
initProcessState :: ProjectContext -> ProcessState
initProcessState context =
  ProcessState {
    packages = updateManagedPackages context.state.bounds (nMap removeLowerBounds context.packages),
    state = context.state
  }
