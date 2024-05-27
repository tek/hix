module Hix.Managed.Cabal.Print where

import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (ResolverPackage (..), SolverInstallPlan, SolverPlanPackage)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (InstalledPackageInfo, sourcePackageId))
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (InstSolverPackage, instSolverPkgIPI))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Exon (exon)

import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId as PackageId
import Hix.Data.PackageId (PackageId)
import qualified Hix.Log as Log
import Hix.Pretty (showP)

resolverPackageId :: ResolverPackage a -> PackageId
resolverPackageId = \case
  PreExisting InstSolverPackage {instSolverPkgIPI = InstalledPackageInfo {sourcePackageId}} ->
    PackageId.fromCabal sourcePackageId
  Configured SolverPackage {solverPkgSource = SourcePackage {srcpkgPackageId}} ->
    PackageId.fromCabal srcpkgPackageId

showPackage :: SolverPlanPackage -> String
showPackage pkg =
  [exon|#{desc}: #{showP (resolverPackageId pkg)}|]
  where
    desc = case pkg of
      PreExisting {} -> "existing"
      Configured {} -> "configured"

printPlan :: SolverInstallPlan -> M ()
printPlan plan = do
  Log.info "Plan:"
  for_ (SolverInstallPlan.toList plan) \ pkg ->
    Log.infoCont [exon|📦 #{toText (showPackage pkg)}|]
