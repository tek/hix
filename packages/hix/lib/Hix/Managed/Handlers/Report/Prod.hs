module Hix.Managed.Handlers.Report.Prod where

import qualified Data.Text as Text
import Data.These (These (..))
import Data.These.Combinators (justHere, justThere)
import Exon (exon)

import qualified Hix.Console
import Hix.Console (color, colors)
import Hix.Data.Monad (M)
import Hix.Data.Version (Version)
import qualified Hix.Data.VersionBounds
import Hix.Data.VersionBounds (VersionBounds (VersionBounds))
import qualified Hix.Log as Log
import qualified Hix.Managed.Data.EnvResult
import Hix.Managed.Data.EnvResult (EnvResult (EnvResult))
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (FailedMutation)
import qualified Hix.Managed.Data.ProjectResult as ProjectResult
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Data.StageResult (StageFailure (FailedMutations, FailedPrecondition), StageSummary (..))
import qualified Hix.Managed.EnvResult
import qualified Hix.Managed.EnvResult as EnvResult
import Hix.Managed.EnvResult (
  BoundsModification (BoundsModification),
  DepModification (DepAdded, DepUpdated),
  DepResult (DepResult),
  DepResultDetail (..),
  DepResults (DepResults),
  )
import qualified Hix.Managed.Handlers.Report
import Hix.Managed.Handlers.Report (ReportHandlers (ReportHandlers))
import Hix.Pretty (showP)

blankLine :: M ()
blankLine = Log.infoPlain ""

listFailed ::
  FailedMutation ->
  Text
listFailed mutation = [exon|📦 #{showP mutation.package}|]

reportFailed ::
  NonEmpty FailedMutation ->
  M ()
reportFailed =
  traverse_ \ mut -> Log.infoCont (listFailed mut)

reportNewVersions ::
  NonEmpty MutableId ->
  M ()
reportNewVersions =
  traverse_ \ MutableId {name, version} ->
    Log.infoCont [exon|📦 '##{name}': #{showP version}|]

printSummary :: StageSummary -> M ()
printSummary = \case
  StageSuccess msg -> Log.info msg
  StageFailure (FailedMutations msg failed) -> do
    Log.info msg
    reportFailed failed
  StageFailure (FailedPrecondition msg) ->
    traverse_ Log.info msg
  StageNoAction msg -> traverse_ Log.info msg
  StageReport msg versions -> do
    Log.info msg
    reportNewVersions versions

printBounds ::
  (Version -> Text) ->
  (Version -> Text) ->
  Maybe Version ->
  Maybe Version ->
  Text
printBounds printL printU lower upper =
  case (lower, upper) of
    (Just l, Just u) -> [exon|[#{printL l}, #{printU u}]|]
    (Just l, Nothing) -> [exon|>=#{printL l}|]
    (Nothing, Just u) -> [exon|<#{printU u}|]
    (Nothing, Nothing) -> "[no bounds]"

formatDepLine :: MutableDep -> Text -> Text -> (Text, Text, Text)
formatDepLine package version bounds =
  ([exon|📦 #{color colors.blue (showP package)}|], version, [exon|↕ #{bounds}|])

formatBoundsModification :: VersionBounds -> BoundsModification -> Text
formatBoundsModification VersionBounds {lower = lowerNew, upper = upperNew} (BoundsModification bm) =
  [exon|#{original} -> #{new}|]
  where

    original = interval lowerOriginal upperOriginal colors.red lowerChanged upperChanged
    new = interval lowerNew upperNew colors.green lowerChanged upperChanged

    lowerOriginal = fromMaybe lowerNew lowerDiff
    upperOriginal = fromMaybe upperNew upperDiff

    lowerChanged = isJust lowerDiff
    upperChanged = isJust upperDiff

    lowerDiff = justHere bm
    upperDiff = justThere bm

    interval l u col colL colU =
      printBounds (bound col colL) (bound col colU) l u

    bound col useCol v
      | useCol
      = color col (showP v)
      | otherwise
      = showP v

formatDep :: MutableDep -> Version -> VersionBounds -> Maybe BoundsModification -> (Text, Text, Text)
formatDep package version bounds boundsMod =
  formatDepLine package (showP version) (maybe unchangedBounds changedBounds boundsMod)
  where
    changedBounds = formatBoundsModification bounds
    unchangedBounds = printBounds showP showP bounds.lower bounds.upper

formatDepUpdate ::
  MutableDep ->
  Version ->
  VersionBounds ->
  These Version BoundsModification ->
  (Text, Text, Text)
formatDepUpdate package version bounds update =
  formatDepLine package versionDesc boundsDesc
  where
    versionDesc = case justHere update of
      Just original -> versionUpdate original version
      Nothing -> showP version

    versionUpdate original new =
      [exon|#{color colors.red (showP original)} -> #{color colors.green (showP new)}|]

    boundsDesc = case justThere update of
      Just boundsMod -> formatBoundsModification bounds boundsMod
      Nothing -> showP bounds

printAligned :: [(Text, Text, Text)] -> M ()
printAligned deps =
  traverse_ printLine deps
  where
    printLine (p, v, b) = Log.infoCont (padded p maxP <> padded v maxV <> b)
    padded s maxlen = s <> Text.replicate (maxlen - Text.length s + 3) " "
    maxP = maxi ps
    maxV = maxi vs
    maxi = fromMaybe 0 . maximum . fmap Text.length
    (ps, vs, _) = unzip3 deps

formatDepResult :: DepResult -> Maybe (Text, Text, Text)
formatDepResult DepResult {..} =
  case detail of
    DepModified (DepAdded boundsMod) -> Just (formatDep package version bounds boundsMod)
    DepModified (DepUpdated update) -> Just (formatDepUpdate package version bounds update)
    DepUnmodified -> Nothing

-- TODO don't print the env name when using @sets = "all"@ (or even when only one env is processed?)
envResult ::
  EnvResult ->
  M ()
envResult result@EnvResult {env, summaries} = do
  Log.info envLabel
  traverse_ printSummary summaries
  for_ (nonEmpty added) \ deps -> do
    Log.info "Added new versions:"
    printAligned (mapMaybe formatDepResult (toList deps))
  for_ (nonEmpty updated) \ deps -> do
    Log.info "Updated versions:"
    printAligned (mapMaybe formatDepResult (toList deps))
  where
    DepResults {..} = EnvResult.grouped result
    envLabel = color colors.yellow (showP env)

mutations ::
  ProjectResult ->
  M ()
mutations results = do
  blankLine
  sequence_ (intersperse blankLine (envResult <$> toList results.envs))

handlersProd :: ReportHandlers
handlersProd =
  ReportHandlers {mutations}
