module Hix.Managed.Report where

import Exon (exon)

import Hix.Data.Version (forNewRange)
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation))
import qualified Hix.Managed.Lower.Data.Bump
import Hix.Managed.Lower.Data.Bump (Bump (Bump))
import Hix.Managed.Lower.Data.LowerInit (LowerInit)
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize)
import Hix.Monad (M)
import Hix.Pretty (showP)

class ReportMutation a where
  reportMutation :: DepMutation a -> M ()

instance ReportMutation Bump where
  reportMutation DepMutation {package, mutation = Bump {..}} = do
    Log.info [exon|  New version for '##{package}': #{showP version}|]
    forNewRange range \ r ->
      Log.info [exon|    New bounds: #{showP r}|]

instance ReportMutation LowerInit where
  reportMutation _ = unit

instance ReportMutation LowerOptimize where
  reportMutation _ = unit

reportMutations ::
  ReportMutation a =>
  [DepMutation a] ->
  M ()
reportMutations mutations
  | null mutations =
    Log.info "All dependencies are up to date."
  | otherwise = do
    Log.info "Found new dependency versions:"
    traverse_ reportMutation mutations
