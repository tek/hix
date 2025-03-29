module Hix.Managed.Cabal.Data.Packages where

import Distribution.Pretty (Pretty (..))
import GHC.Exts (IsList)

import Hix.Data.PackageId (PackageId)
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)
import Hix.Pretty (HPretty (..), field, prettyMap)

newtype InstalledPackages =
  InstalledPackages [(PackageId, [PackageId])]
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsList, Semigroup, Monoid, HPretty)

-- | Mock data for package indexes for the solver.
--
-- Installed packages should not contain local packages, since they have fixed dependency versions.
-- Instead, they should be included in the available packages, but this is done automatically by the @SolveResources@
-- initializer, so these here correspond only to Hackage packages.
data GhcPackages =
  GhcPackages {
    installed :: InstalledPackages,
    available :: SourcePackages
  }
  deriving stock (Eq, Show, Generic)

instance Pretty GhcPackages where
  pretty GhcPackages {..} =
    prettyMap "Mock packages" [
      field "installed" (hpretty installed),
      field "available" (hpretty available)
    ]
