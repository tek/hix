module Hix.Managed.Solve.Changes where

import Data.List (partition)
import qualified Data.Map.Strict as Map
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

import Hix.Data.Overrides (Override (..))
import Hix.Data.Package (PackageName, packageNameFromCabal)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion))
import qualified Hix.Log as Log
import qualified Hix.Managed.Handlers.Hackage
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import Hix.Data.Monad (M)
import Hix.Pretty (showPL)

data SolverPlan =
  SolverPlan {
    configured :: [NewVersion],
    preexisting :: [NewVersion]
  }
  deriving stock (Eq, Show, Generic)

solverPlan ::
  SolverInstallPlan ->
  SolverPlan
solverPlan plan =
  uncurry SolverPlan (partitionEithers (mkVersion <$> SolverInstallPlan.toList plan))
  where
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
    projectDeps :: [NewVersion],
    overrides :: Map PackageName Override
  }
  deriving stock (Eq, Show, Generic)

changedDeps ::
  Set PackageName ->
  SolverPlan ->
  ([NewVersion], [NewVersion])
changedDeps allDeps SolverPlan {..} =
  (configured, filter isDep allVersions)
  where
    isDep nv = Set.member nv.package allDeps
    allVersions = configured ++ preexisting

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

processSolverPlan :: Set PackageName -> HackageHandlers -> SolverPlan -> M SolverChanges
processSolverPlan allDeps handlers plan = do
  Log.debug [exon|New project deps from solver: #{showPL projectDeps}|]
  overrides <- for accepted \ NewVersion {..} -> do
    hash <- handlers.fetchHash package version
    pure (package, Override {..})
  when (not (null rejected)) do
    Log.verbose [exon|NOTE: Cabal solver suggested new versions for non-reinstallable packages: #{bad}|]
  pure SolverChanges {overrides = Map.fromList overrides, versions = accepted, projectDeps}
  where
    bad = show (hsep (pretty <$> rejected))
    (rejected, accepted) = partition (flip Set.member nonReinstallable . (.package)) changed
    (changed, projectDeps) = changedDeps allDeps plan
