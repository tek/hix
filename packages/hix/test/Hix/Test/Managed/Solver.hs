module Hix.Test.Managed.Solver where

import Data.List.Extra (nubOrdOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import Distribution.Version (Version, VersionRange, withinRange)
import Exon (exon)

import Hix.Class.Map (ntList, via, (!!))
import Hix.Data.Package (Package (..), PackageName)
import qualified Hix.Log as Log
import Hix.Managed.Data.ManagedConfig (ManagedOp)
import qualified Hix.Managed.Data.SolverParams as SolverParams
import Hix.Managed.Data.SolverParams (SolverParams)
import qualified Hix.Managed.Solve.Changes
import Hix.Managed.Solve.Changes (SolverPlan (SolverPlan))
import Hix.Monad (M, noteFatal)
import Hix.Pretty (showP)

data TestDeps =
  TestDeps {
    fetchVersions :: (PackageName -> M [Version]),
    byPackage :: Map PackageName [(PackageName, VersionRange)],
    byVersion :: Map (PackageName, Version) [(PackageName, VersionRange)]
  }

trySolvedVersion ::
  (PackageName -> M [Version]) ->
  ManagedOp ->
  Bool ->
  PackageName ->
  VersionRange ->
  M (Maybe Package)
trySolvedVersion fetchVersions _ oldest package range = do
  versions <- fetchVersions package
  let pool | oldest = versions
           | otherwise = reverse versions
  pure $ find (flip withinRange range) pool <&> \ version -> Package {name = package, version}

solvedVersion ::
  (PackageName -> M [Version]) ->
  ManagedOp ->
  SolverParams ->
  PackageName ->
  VersionRange ->
  M Package
solvedVersion fetchVersions op params package range = do
  result <- trySolvedVersion fetchVersions op oldest package range
  noteFatal [exon|Couldn't solve for ##{package} | #{showP range}|] result
  where
    oldest = (params !! package).oldest

testSolver ::
  TestDeps ->
  ManagedOp ->
  SolverParams ->
  Package ->
  M (Maybe SolverPlan)
testSolver testDeps op params candidate
  | Just range <- bounds !! candidate.name
  , not (withinRange candidate.version range)
  = pure Nothing
  | otherwise
  = do
    Log.debug [exon|Test solver bounds: #{showP bounds}|]
    direct <- (candidate :) <$> solve (ntList (via (Map.delete candidate.name) bounds))
    let
      transitiveRanges =
        [v | d <- direct, v <- fold (testDeps.byVersion !? (d.name, d.version) <|> testDeps.byPackage !? d.name)]
    transitive <- solve transitiveRanges
    let
      overrides = nubOrdOn (.name) (transitive ++ direct)
      resolvedCandidate = find (\ o -> o.name == candidate.name) overrides
    pure if
      | Just rc <- resolvedCandidate, rc.version /= candidate.version -> Nothing
      | otherwise -> Just SolverPlan {overrides, matching = []}
  where
    solve = traverse (uncurry (solvedVersion testDeps.fetchVersions op params))
    bounds = SolverParams.toBounds op params
