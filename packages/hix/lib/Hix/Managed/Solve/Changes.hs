module Hix.Managed.Solve.Changes where

import Data.List (partition)
import qualified Data.Set as Set
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (ResolverPackage (Configured, PreExisting), SolverInstallPlan)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (..))
import Distribution.Package (PackageIdentifier (PackageIdentifier))
import Distribution.Pretty (pretty)
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (..))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Exon (exon)
import Text.PrettyPrint (hsep)

import Hix.Data.Monad (M)
import Hix.Data.Package (PackageName, packageNameFromCabal)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import qualified Hix.Log as Log
import Hix.Pretty (showPL)

data SolverPlan =
  SolverPlan {
    -- | Deps whose versions differ from those in the package db, i.e. where the GHC set from nixpkgs has a different
    -- version.
    -- We need to add Nix overrides for these.
    overrides :: [NewVersion],
    -- | Versions that match those in the package db.
    -- We don't need to add Nix overrides for these.
    matching :: [NewVersion]
  }
  deriving stock (Eq, Show, Generic)

solverPlan ::
  SolverInstallPlan ->
  SolverPlan
solverPlan plan =
  SolverPlan {overrides, matching}
  where
    (overrides, matching) = partitionEithers (mkVersion <$> SolverInstallPlan.toList plan)

    mkVersion = \case
      Configured SolverPackage {solverPkgSource = SourcePackage {srcpkgPackageId = PackageIdentifier name version}} ->
        Left NewVersion {package = packageNameFromCabal name, version}
      PreExisting InstSolverPackage {
        instSolverPkgIPI = InstalledPackageInfo {sourcePackageId = PackageIdentifier name version}
      } ->
        Right NewVersion {package = packageNameFromCabal name, version}

data SolverChanges =
  SolverChanges {
    versions :: [NewVersion],
    projectDeps :: [NewVersion]
  }
  deriving stock (Eq, Show, Generic)

changedDeps ::
  Set PackageName ->
  SolverPlan ->
  ([NewVersion], [NewVersion])
changedDeps allDeps SolverPlan {..} =
  (overrides, filter isDep allVersions)
  where
    isDep nv = Set.member nv.package allDeps
    allVersions = overrides ++ matching

nonReinstallable :: Set PackageName
nonReinstallable =
  [
    "base",
    "ghc-bignum",
    "ghc-prim",
    "ghc",
    "integer-gmp",
    "integer-simple",
    "template-haskell"
  ]

processSolverPlan :: Set PackageName -> SolverPlan -> M SolverChanges
processSolverPlan allDeps plan = do
  Log.debug [exon|New project deps from solver: #{showPL projectDeps}|]
  when (not (null rejected)) do
    Log.verbose [exon|NOTE: Cabal solver suggested new versions for non-reinstallable packages: #{bad}|]
  pure SolverChanges {versions = accepted, projectDeps}
  where
    bad = show (hsep (pretty <$> rejected))
    (rejected, accepted) = partition (flip Set.member nonReinstallable . (.package)) changed
    (changed, projectDeps) = changedDeps allDeps plan
