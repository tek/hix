module Hix.Managed.Lower.Candidates where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Distribution.Pretty (pretty)
import Distribution.Version (Version, VersionRange)
import Exon (exon)

import Hix.Class.Map (ntMap)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import Hix.Data.Package (PackageName)
import Hix.Data.Version (Major, Versions)
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation))
import qualified Hix.Managed.Lower.Data.Lower
import Hix.Managed.Lower.Data.Lower (Lower (Lower))
import Hix.Monad (throwM)
import Hix.Pretty (showP)
import Hix.Version (allMajors, lowerBound, majorParts, majorsBefore, upperBound, versionsBetween, versionsFrom)

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

specifiedLower :: VersionRange -> Maybe (Int, Int)
specifiedLower = majorParts <=< lowerBound

specifiedUpper :: VersionRange -> Maybe (Int, Int)
specifiedUpper = majorParts <=< upperBound

prefix :: Int -> Int -> Text
prefix s m = [exon|#{show s}.#{show m}|]

candidates ::
  (PackageName -> M [Version]) ->
  Dep ->
  ([Version] -> Maybe (NonEmpty Major)) ->
  M (Maybe (DepMutation Lower))
candidates fetchVersions dep selection = do
  allVersions <- fetchVersions dep.package
  let
    result = do
      majors <- selection (sort allVersions)
      pure DepMutation {package, mutation = Lower {majors, range = dep.version}}
  logNoVersions package allVersions result
  pure result
  where
    package = dep.package

data InitConfig =
  InitBeforeUpper Int Int
  |
  InitAll
  deriving stock (Eq, Show, Generic)

initConfig :: VersionRange -> InitConfig
initConfig version =
  case specifiedUpper version of
    Just (s, m) -> InitBeforeUpper s m
    _ -> InitAll

logInitConfig :: PackageName -> InitConfig -> M ()
logInitConfig package conf =
  Log.verbose [exon|Choosing versions for '##{package}' from #{msg conf}.|]
  where
    msg = \case
      InitBeforeUpper s m ->
        [exon|all majors before the specified upper bound #{prefix s m}|]
      InitAll ->
        "all majors."

selectionInit :: InitConfig -> [Version] -> Maybe (NonEmpty Major)
selectionInit = \case
  InitBeforeUpper s m ->
    nonEmpty . reverse . majorsBefore s m
  InitAll ->
    nonEmpty . reverse . allMajors

candidatesInit ::
  (PackageName -> M [Version]) ->
  Versions ->
  Dep ->
  M (Maybe (DepMutation Lower))
candidatesInit fetchVersions pre dep
  | Map.member dep.package (ntMap pre)
  = pure Nothing
  | otherwise
  = do
    logInitConfig dep.package conf
    candidates fetchVersions dep (selectionInit conf)
  where
    conf = initConfig dep.version

data OptimizeConfig =
  OptimizeMajorsBefore Int Int
  |
  OptimizeNoBound
  deriving stock (Eq, Show, Generic)

optimizeConfig :: VersionRange -> OptimizeConfig
optimizeConfig version =
  case specifiedLower version of
    Just (s, m) -> OptimizeMajorsBefore s m
    _ -> OptimizeNoBound

logOptimizeConfig :: PackageName -> OptimizeConfig -> M ()
logOptimizeConfig package = \case
  OptimizeMajorsBefore s m ->
    Log.verbose [exon|Choosing versions for '##{package}' from all majors before #{prefix s m}.|]
  OptimizeNoBound ->
    throwM (Client [exon|'##{package}' has no lower bound. Please run '.#lower.init' first.|])

selectionOptimize :: OptimizeConfig -> [Version] -> Maybe (NonEmpty Major)
selectionOptimize = \case
  OptimizeMajorsBefore s m -> do
    nonEmpty . reverse . majorsBefore s m
  OptimizeNoBound ->
    const Nothing

candidatesOptimize ::
  (PackageName -> M [Version]) ->
  Dep ->
  M (Maybe (DepMutation Lower))
candidatesOptimize fetchVersions dep = do
  logOptimizeConfig dep.package conf
  candidates fetchVersions dep (selectionOptimize conf)
  where
    conf = optimizeConfig dep.version

data StabilizeConfig =
  StabilizeFromVersion Version (Maybe Version)
  |
  StabilizeNoBound
  deriving stock (Eq, Show, Generic)

stabilizeConfig :: VersionRange -> Maybe Version -> StabilizeConfig
stabilizeConfig version initialBound =
  case lowerBound version of
    Just v -> StabilizeFromVersion v (initialBound <|> upperBound version)
    _ -> StabilizeNoBound

logStabilizeConfig :: PackageName -> StabilizeConfig -> M ()
logStabilizeConfig package = \case
  StabilizeFromVersion v Nothing ->
    Log.verbose [exon|Choosing versions for '##{package}' after the current version #{showP v}, if it doesn't build.|]
  StabilizeFromVersion l (Just u) ->
    Log.verbose [exon|Choosing versions for '##{package}' between the current version #{showP l} and the initial version #{showP u}, if it doesn't build.|]
  StabilizeNoBound ->
    throwM (Client [exon|'##{package}' has no lower bound. Please run '.#lower.init' first.|])

selectionStabilize :: StabilizeConfig -> [Version] -> Maybe (NonEmpty Major)
selectionStabilize = \case
  StabilizeFromVersion l Nothing ->
    nonEmpty . versionsFrom l
  StabilizeFromVersion l (Just u) ->
    nonEmpty . versionsBetween l u
  StabilizeNoBound ->
    const Nothing

candidatesStabilize ::
  (PackageName -> M [Version]) ->
  Dep ->
  Maybe Version ->
  M (Maybe (DepMutation Lower))
candidatesStabilize fetchVersions dep initialBound = do
  logStabilizeConfig dep.package conf
  candidates fetchVersions dep (selectionStabilize conf)
  where
    conf = stabilizeConfig dep.version initialBound
