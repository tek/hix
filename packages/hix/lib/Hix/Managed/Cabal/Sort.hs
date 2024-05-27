module Hix.Managed.Cabal.Sort where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Distribution.Client.SolverInstallPlan (topologicalOrder)
import Exon (exon)

import Hix.Data.Monad (M)
import qualified Hix.Data.PackageId
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.SolveResources (SolveResources)
import Hix.Managed.Cabal.Print (resolverPackageId)
import Hix.Managed.Cabal.Solve (solveForTargets)
import Hix.Managed.Cabal.Targets (solveTarget)
import Hix.Managed.Data.Mutable (MutableDep (MutableDep))
import Hix.Managed.Data.Mutation (DepMutation (..))
import Hix.Data.PackageName (PackageName)

sortDeps ::
  ∀ a .
  SolveResources ->
  (a -> PackageName) ->
  [a] ->
  M [a]
sortDeps resources name mutations =
  solveForTargets resources id targets >>= \case
    Right plan ->
      pure (mapMaybe original (topologicalOrder plan))
    Left err -> do
      Log.error [exon|No plan for mutations: ##{err}|]
      pure mutations
  where
    original k = byName !? (resolverPackageId k).name
    byName = Map.fromList [(name m, m) | m <- mutations]
    targets = [solveTarget (name m) mempty | m <- mutations]

sortMutations ::
  ∀ a .
  SolveResources ->
  [DepMutation a] ->
  M [DepMutation a]
sortMutations resources =
  sortDeps resources name
  where
    name DepMutation {package = MutableDep package} = package
