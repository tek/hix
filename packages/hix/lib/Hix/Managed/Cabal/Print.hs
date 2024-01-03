module Hix.Managed.Cabal.Print where

import qualified Distribution.Client.SolverInstallPlan as SolverInstallPlan
import Distribution.Client.SolverInstallPlan (ResolverPackage (..), SolverInstallPlan, SolverPlanPackage)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (InstalledPackageInfo, sourcePackageId))
import Distribution.Pretty (pretty)
import Distribution.Solver.Types.InstSolverPackage (InstSolverPackage (InstSolverPackage, instSolverPkgIPI))
import Distribution.Solver.Types.SolverPackage (SolverPackage (..))
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Exon (exon)

import Hix.Data.Monad (M)
import qualified Hix.Log as Log

showPackage :: SolverPlanPackage -> String
showPackage = \case
  PreExisting InstSolverPackage {instSolverPkgIPI = InstalledPackageInfo {sourcePackageId}} ->
    "existing: " <> show (pretty sourcePackageId)
  Configured SolverPackage {solverPkgSource = SourcePackage {srcpkgPackageId}} ->
    "configured: " <> show (pretty srcpkgPackageId)

printPlan :: SolverInstallPlan -> M ()
printPlan plan = do
  Log.info "Plan:"
  for_ (SolverInstallPlan.toList plan) \ pkg ->
    Log.infoCont [exon|📦 #{toText (showPackage pkg)}|]
