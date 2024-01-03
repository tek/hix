module Hix.Managed.Lower.Candidates where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Distribution.Pretty (pretty)
import Distribution.Version (Version)
import Exon (exon)

import Hix.Class.Map ((!!))
import Hix.Data.Error (Error (Client))
import Hix.Data.Monad (M)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Major)
import qualified Hix.Data.VersionBounds
import Hix.Data.VersionBounds (VersionBounds)
import qualified Hix.Log as Log
import qualified Hix.Managed.Data.Lower
import Hix.Managed.Data.Lower (Lower (Lower))
import Hix.Managed.Data.Mutable (MutableDep, MutableVersions, depName)
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (DepMutation (DepMutation))
import qualified Hix.Managed.Data.QueryDep
import Hix.Managed.Data.QueryDep (QueryDep)
import Hix.Monad (throwM)
import Hix.Pretty (showP)
import Hix.Version (allMajors, majorParts, majorsBefore, versionsBetween, versionsFrom)

logNoVersions ::
  MutableDep ->
  [Version] ->
  Maybe a ->
  M (Maybe a)
logNoVersions package allVersions mutation = do
  when (isNothing mutation) do
    Log.debug [exon|Available versions: #{Text.intercalate ", " (show . pretty <$> allVersions)}|]
    Log.warn [exon|No suitable version found for '##{package}'.|]
  pure mutation

specifiedLower :: VersionBounds -> Maybe (Int, Int)
specifiedLower = majorParts <=< (.lower)

specifiedUpper :: VersionBounds -> Maybe (Int, Int)
specifiedUpper = majorParts <=< (.upper)

prefix :: Int -> Int -> Text
prefix s m = [exon|#{show s}.#{show m}|]

candidates ::
  (PackageName -> M [Version]) ->
  QueryDep ->
  Bool ->
  ([Version] -> Maybe (NonEmpty Major)) ->
  M (Maybe (DepMutation Lower))
candidates fetchVersions dep retract selection = do
  allVersions <- fetchVersions (depName package)
  let
    result = do
      majors <- selection (sort allVersions)
      pure DepMutation {package, retract, mutation = Lower {majors}}
  logNoVersions package allVersions result
  pure result
  where
    package = dep.package

data InitConfig =
  InitBeforeUpper Int Int
  |
  InitAll
  deriving stock (Eq, Show, Generic)

initConfig :: VersionBounds -> InitConfig
initConfig version =
  case specifiedUpper version of
    Just (s, m) -> InitBeforeUpper s m
    _ -> InitAll

logInitConfig :: MutableDep -> InitConfig -> M ()
logInitConfig package conf =
  Log.verbose [exon|Choosing versions for '##{package}' from #{msg conf}.|]
  where
    msg = \case
      InitBeforeUpper s m ->
        [exon|all majors before the specified upper bound #{prefix s m}|]
      InitAll ->
        "all majors"

selectionInit :: InitConfig -> [Version] -> Maybe (NonEmpty Major)
selectionInit = \case
  InitBeforeUpper s m ->
    nonEmpty . reverse . majorsBefore s m
  InitAll ->
    nonEmpty . reverse . allMajors

candidatesInit ::
  (PackageName -> M [Version]) ->
  Set MutableDep ->
  QueryDep ->
  M (Maybe (DepMutation Lower))
candidatesInit fetchVersions pre dep
  | Set.member dep.package pre
  = pure Nothing
  | otherwise
  = do
    logInitConfig dep.package conf
    candidates fetchVersions dep False (selectionInit conf)
  where
    conf = initConfig dep.version

data OptimizeConfig =
  OptimizeMajorsBefore Int Int
  |
  OptimizeNoBound
  deriving stock (Eq, Show, Generic)

optimizeConfig ::
  Maybe Version ->
  VersionBounds ->
  OptimizeConfig
optimizeConfig initial version =
  case majorParts =<< (version.lower <|> initial) of
    Just (s, m) -> OptimizeMajorsBefore s m
    Nothing -> OptimizeNoBound

logOptimizeConfig ::
  MutableDep ->
  OptimizeConfig ->
  M ()
logOptimizeConfig package = \case
  OptimizeMajorsBefore s m ->
    Log.verbose [exon|Choosing versions for '##{package}' from all majors before #{prefix s m}.|]
  OptimizeNoBound ->
    throwM (Client [exon|'##{package}' has no initial lower bound. Please run '.#lower.init' first.|])

selectionOptimize :: OptimizeConfig -> [Version] -> Maybe (NonEmpty Major)
selectionOptimize = \case
  OptimizeMajorsBefore s m -> do
    nonEmpty . reverse . majorsBefore s m
  OptimizeNoBound ->
    const Nothing

candidatesOptimize ::
  (PackageName -> M [Version]) ->
  MutableVersions ->
  QueryDep ->
  M (Maybe (DepMutation Lower))
candidatesOptimize fetchVersions initial dep = do
  logOptimizeConfig dep.package conf
  candidates fetchVersions dep False (selectionOptimize conf)
  where
    conf = optimizeConfig (join (initial !! dep.package)) dep.version

data StabilizeConfig =
  StabilizeFromVersion Version (Maybe Version)
  |
  StabilizeNoBound InitConfig
  deriving stock (Eq, Show, Generic)

stabilizeConfig :: VersionBounds -> Maybe Version -> StabilizeConfig
stabilizeConfig version initialBound =
  case version.lower of
    Just v -> StabilizeFromVersion v (initialBound <|> version.upper)
    Nothing | Just v <- initialBound -> StabilizeFromVersion v (initialBound <|> version.upper)
    Nothing -> StabilizeNoBound (initConfig version)

logStabilizeConfig :: MutableDep -> StabilizeConfig -> M ()
logStabilizeConfig package = \case
  StabilizeFromVersion v Nothing ->
    Log.verbose [exon|Choosing versions for '##{package}' after the current version #{showP v}.|]
  StabilizeFromVersion l (Just u) ->
    Log.verbose [exon|Choosing versions for '##{package}' between the current version #{showP l} and the initial version #{showP u}.|]
  StabilizeNoBound conf ->
    logInitConfig package conf

selectionStabilize :: StabilizeConfig -> [Version] -> Maybe (NonEmpty Major)
selectionStabilize = \case
  StabilizeFromVersion l Nothing ->
    nonEmpty . versionsFrom l
  StabilizeFromVersion l (Just u) ->
    nonEmpty . versionsBetween l u
  StabilizeNoBound conf ->
    selectionInit conf

candidatesStabilize ::
  (PackageName -> M [Version]) ->
  QueryDep ->
  Maybe Version ->
  M (Maybe (DepMutation Lower))
candidatesStabilize fetchVersions dep initialBound = do
  logStabilizeConfig dep.package conf
  candidates fetchVersions dep (isRetract conf) (selectionStabilize conf)
  where
    conf = stabilizeConfig dep.version initialBound

    isRetract = \case
      StabilizeFromVersion _ _ -> True
      StabilizeNoBound _ -> False
