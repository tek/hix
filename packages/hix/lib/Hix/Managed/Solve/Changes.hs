module Hix.Managed.Solve.Changes where

import Data.List (partition)
import qualified Data.Set as Set
import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (ResolverPackage (Configured, PreExisting), SolverInstallPlan)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (..))
import Distribution.Pretty (Pretty, pretty)
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (..))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Exon (exon)
import Text.PrettyPrint (hang, hsep, ($+$))

import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import qualified Hix.Log as Log
import Hix.Pretty (prettyL, showPL)

data SolverPlan =
  SolverPlan {
    -- | Deps whose versions differ from those in the package db, i.e. where the GHC set from nixpkgs has a different
    -- version.
    -- We need to add Nix overrides for these.
    overrides :: [PackageId],
    -- | Versions that match those in the package db.
    -- We don't need to add Nix overrides for these.
    matching :: [PackageId]
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
      Configured SolverPackage {solverPkgSource = SourcePackage {srcpkgPackageId}} ->
        Left (PackageId.fromCabal srcpkgPackageId)
      PreExisting InstSolverPackage {instSolverPkgIPI = InstalledPackageInfo {sourcePackageId}} ->
        Right (PackageId.fromCabal sourcePackageId)

data SolverChanges =
  SolverChanges {
    overrides :: [PackageId],
    projectDeps :: [PackageId]
  }
  deriving stock (Eq, Show, Generic)

instance Pretty SolverChanges where
  pretty SolverChanges {..} =
    hang "overrides:" 2 (prettyL overrides) $+$ hang "projectDeps:" 2 (prettyL projectDeps)

changedDeps ::
  Set PackageName ->
  SolverPlan ->
  ([PackageId], [PackageId])
changedDeps allDeps SolverPlan {..} =
  (overrides, filter isDep allVersions)
  where
    isDep package = Set.member package.name allDeps
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
  pure SolverChanges {overrides = accepted, projectDeps}
  where
    bad = show (hsep (pretty <$> rejected))
    (rejected, accepted) = partition (flip Set.member nonReinstallable . (.name)) changed
    (changed, projectDeps) = changedDeps allDeps plan
