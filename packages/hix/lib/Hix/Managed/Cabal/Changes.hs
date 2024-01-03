module Hix.Managed.Cabal.Changes where

import Data.List (partition)
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (ResolverPackage (Configured, PreExisting), SolverInstallPlan)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (..))
import Distribution.Pretty (Pretty, pretty)
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (..))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Text.PrettyPrint (hang, ($+$))

import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Config (isNonReinstallableId)
import Hix.Pretty (prettyL)

-- | Raw results from Cabal, which include local packages in the overrides.
data SolverPlan =
  SolverPlan {
    -- | Deps whose versions differ from those in the package db, i.e. where the GHC set from nixpkgs has a different
    -- version.
    -- We need to add Nix overrides for these.
    changes :: [PackageId],

    -- | Versions that match those in the package db.
    -- We don't need to add Nix overrides for these.
    matching :: [PackageId],

    -- | GHC boot packages that were selected for reinstallation, which is an error with the default settings.
    nonReinstallable :: Maybe (NonEmpty PackageId)
  }
  deriving stock (Eq, Show, Generic)

instance Pretty SolverPlan where
  pretty SolverPlan {changes, matching} =
    hang "changes:" 2 (prettyL changes) $+$ hang "matching:" 2 (prettyL matching)

solverPlan ::
  SolverInstallPlan ->
  SolverPlan
solverPlan plan =
  SolverPlan {..}
  where
    (nonEmpty -> nonReinstallable, changes) = partition isNonReinstallableId configured

    (configured, matching) = partitionEithers (mkVersion <$> SolverInstallPlan.toList plan)

    mkVersion = \case
      Configured SolverPackage {solverPkgSource = SourcePackage {srcpkgPackageId}} ->
        Left (PackageId.fromCabal srcpkgPackageId)
      PreExisting InstSolverPackage {instSolverPkgIPI = InstalledPackageInfo {sourcePackageId}} ->
        Right (PackageId.fromCabal sourcePackageId)

data SolverChanges =
  SolverChanges {
    versions :: Versions,
    overrides :: [PackageId],
    projectDeps :: Versions
  }
  deriving stock (Eq, Show, Generic)

instance Pretty SolverChanges where
  pretty SolverChanges {..} =
    hang "overrides:" 2 (prettyL overrides) $+$
    hang "projectDeps:" 2 (pretty projectDeps)
