module Hix.Test.Managed.Solver where

import Data.List.Extra (nubOrdOn)
import Data.Map.Strict ((!?))
import Distribution.Version (Version, VersionRange, hasUpperBound, withinRange)
import Exon (exon)

import Hix.Class.Map (ntList, (!!))
import Hix.Data.Bounds (Bounds)
import Hix.Data.Package (PackageName)
import Hix.Data.Version (NewVersion (..))
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
  PackageName ->
  VersionRange ->
  M (Maybe NewVersion)
trySolvedVersion fetchVersions package range = do
  versions <- fetchVersions package
  let pool | hasUpperBound range = reverse versions
           | otherwise = versions
  pure $ find (flip withinRange range) pool <&> \ version -> NewVersion {package, version}

solvedVersion ::
  (PackageName -> M [Version]) ->
  PackageName ->
  VersionRange ->
  M NewVersion
solvedVersion fetchVersions package range = do
  result <- trySolvedVersion fetchVersions package range
  noteFatal [exon|Couldn't solve for ##{package} | #{showP range}|] result

testSolver ::
  TestDeps ->
  Bounds ->
  NewVersion ->
  M (Maybe SolverPlan)
testSolver testDeps bounds candidate
  | Just range <- bounds !! candidate.package
  , not (withinRange candidate.version range)
  = pure Nothing
  | otherwise
  = do
    direct <- (candidate :) <$> solve (ntList bounds)
    let
      -- TODO should this not be <> instead of <|>?
      transitiveRanges =
        [v | d <- direct, v <- fold (testDeps.byVersion !? (d.package, d.version) <|> testDeps.byPackage !? d.package)]
    transitive <- solve transitiveRanges
    let overrides = nubOrdOn (.package) (transitive ++ direct)
    pure (Just SolverPlan {overrides, matching = []})
  where
    solve = traverse (uncurry (solvedVersion testDeps.fetchVersions))
