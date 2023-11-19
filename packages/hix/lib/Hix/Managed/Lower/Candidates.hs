module Hix.Managed.Lower.Candidates where

import qualified Data.Text as Text
import Distribution.Pretty (pretty)
import Distribution.Version (Version, VersionRange)
import Exon (exon)

import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Monad (M)
import Hix.Data.Package (PackageName)
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation))
import qualified Hix.Managed.Lower.Data.LowerInit
import Hix.Managed.Lower.Data.LowerInit (LowerInit (LowerInit))
import qualified Hix.Managed.Lower.Data.LowerOptimize
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize (LowerOptimize))
import Hix.Version (allMajors, lowerBound, majorParts, majorsBefore, majorsFrom, onlyMajor)

logNoVersions ::
  PackageName ->
  [Version] ->
  Maybe a ->
  M (Maybe a)
logNoVersions package allVersions mutation = do
  when (isNothing mutation) do
    Log.debug [exon|Available versions: #{Text.intercalate ", " (show . pretty <$> allVersions)}|]
    Log.warn [exon|No suitable version found for '##{package}'.|]
  pure mutation

toDepMutation ::
  PackageName ->
  [Version] ->
  Maybe a ->
  M (Maybe (DepMutation a))
toDepMutation package allVersions result =
  logNoVersions package allVersions (result <&> \ mutation -> DepMutation {package, mutation})

data InitConfig =
  InitLowerMajor Int Int
  |
  InitFromLowerMajor Int Int
  |
  InitAll
  deriving stock (Eq, Show, Generic)

initConfig ::
  Bool ->
  VersionRange ->
  InitConfig
initConfig onlyLowerMajor version =
  case specifiedLower of
    Just (s, m) | onlyLowerMajor -> InitLowerMajor s m
                | otherwise -> InitFromLowerMajor s m
    _ -> InitAll
  where
    specifiedLower = majorParts =<< lowerBound version

prefix :: Int -> Int -> Text
prefix s m = [exon|#{show s}.#{show m}|]

logInitConfig :: PackageName -> InitConfig -> M ()
logInitConfig package conf =
  Log.verbose [exon|Choosing versions for '##{package}' from #{desc}|]
  where
    desc = case conf of
      InitLowerMajor s m ->
        [exon|the major #{prefix s m} of the specified lower bound|]
      InitFromLowerMajor s m ->
        [exon|the major #{prefix s m} of the specified lower bound and later, then all lower versions|]
      InitAll ->
        "all versions"

chooseInit ::
  [Version] ->
  InitConfig ->
  Dep ->
  Maybe LowerInit
chooseInit allVersions conf dep = do
  majors <- selection (sort allVersions)
  pure LowerInit {majors, range = dep.version}
  where
    selection = case conf of
      InitLowerMajor s m -> fmap pure . onlyMajor s m
      InitFromLowerMajor s m -> \ vs -> nonEmpty (majorsFrom s m vs ++ reverse (majorsBefore s m vs))
      InitAll -> nonEmpty . reverse . allMajors

candidatesInit ::
  (PackageName -> M [Version]) ->
  Bool ->
  Dep ->
  M (Maybe (DepMutation LowerInit))
candidatesInit fetchVersions onlyLowerMajor dep = do
  logInitConfig package conf
  allVersions <- fetchVersions dep.package
  let chosen = chooseInit allVersions conf dep
  toDepMutation package allVersions chosen
  where
    conf = initConfig onlyLowerMajor dep.version
    package = dep.package

data OptimizeConfig =
  OptimizeMajorsBefore Int Int
  |
  OptimizeNoBound
  deriving stock (Eq, Show, Generic)

optimizeConfig :: VersionRange -> OptimizeConfig
optimizeConfig version =
  case lowerBound version of
    Just (majorParts -> Just (s, m)) -> OptimizeMajorsBefore s m
    _ -> OptimizeNoBound

logOptimizeConfig :: PackageName -> OptimizeConfig -> M ()
logOptimizeConfig package = \case
  OptimizeMajorsBefore s m ->
    Log.verbose [exon|Choosing versions for '##{package}' from all majors before #{prefix s m}.|]
  OptimizeNoBound ->
    Log.warn [exon|Skipping '##{package}' since it has no configured lower bound. Please run '.#lower.init' first.|]

chooseOptimize ::
  [Version] ->
  Dep ->
  OptimizeConfig ->
  Maybe LowerOptimize
chooseOptimize allVersions dep = \case
  OptimizeMajorsBefore s m -> do
    majors <- nonEmpty (majorsBefore s m (sort allVersions))
    pure LowerOptimize {majors, range = dep.version}
  OptimizeNoBound ->
    Nothing

candidatesOptimize ::
  (PackageName -> M [Version]) ->
  Dep ->
  M (Maybe (DepMutation LowerOptimize))
candidatesOptimize fetchVersions dep = do
  logOptimizeConfig package conf
  allVersions <- fetchVersions dep.package
  let chosen = chooseOptimize allVersions dep conf
  toDepMutation package allVersions chosen
  where
    conf = optimizeConfig dep.version
    package = dep.package
