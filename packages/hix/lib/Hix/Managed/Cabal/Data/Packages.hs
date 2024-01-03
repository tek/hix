module Hix.Managed.Cabal.Data.Packages where

import Hix.Data.PackageId (PackageId)
import Hix.Managed.Cabal.Data.SourcePackage (SourcePackages)

-- | Mock data for package indexes for the solver.
--
-- Installed packages should not contain local packages, since they have fixed dependency versions.
-- Instead, they should be included in the available packages, but this is done automatically by the @SolveResources@
-- initializer, so these here correspond only to Hackage packages.
data GhcPackages =
  GhcPackages {
    installed :: [(PackageId, [PackageId])],
    available :: SourcePackages
  }
  deriving stock (Eq, Show, Generic)
