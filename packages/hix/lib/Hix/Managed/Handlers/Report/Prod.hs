module Hix.Managed.Handlers.Report.Prod where

import qualified Data.Map.Strict as Map
import Exon (exon)

import qualified Hix.Data.Bounds
import Hix.Data.Bounds (RemovableBounds)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import qualified Hix.Data.Package
import Hix.Data.Package (Package (Package), PackageName)
import Hix.Data.Version (NewRange, renderNewRange)
import qualified Hix.Log as Log
import Hix.Managed.Build.Mutation (DepMutation, RenderMutation (renderMutation))
import qualified Hix.Managed.Data.BuildResult
import Hix.Managed.Data.BuildResult (BuildChanges, BuildResult (..))
import qualified Hix.Managed.Data.BuildResults as BuildResults
import Hix.Managed.Data.BuildResults (BuildResults)
import Hix.Managed.Data.BuildState (modifiedCandidates)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import qualified Hix.Managed.Handlers.Report
import Hix.Managed.Handlers.Report (ReportHandlers (ReportHandlers))
import Hix.Pretty (showP)

listPkg :: PackageName -> Text -> NewRange -> Text
listPkg package version range =
  [exon|📦 ##{package} #{version} [#{renderNewRange range}]|]

listSuccess :: Candidate -> Text
listSuccess Candidate {package = Package {name, version}, range} =
  listPkg name (showP version) range

listFailed ::
  RenderMutation a =>
  DepMutation a ->
  Text
listFailed mutation = [exon|📦 #{renderMutation mutation}|]

reportSuccesses ::
  Foldable f =>
  BuildChanges f ->
  M ()
reportSuccesses changes =
  for_ (nonEmpty (modifiedCandidates (toList changes.success))) \ success -> do
    Log.info "Updated dependency versions:"
    for_ success \ p ->
      Log.infoCont (listSuccess p)

reportRemovable :: RemovableBounds -> M ()
reportRemovable removable = do
  for_ (Map.toList removable.deps) \ (target, deps) -> do
    Log.info [exon|You can remove the #{showP removable.targetBound} bounds from these deps of '##{target}':|]
    for_ deps \ dep ->
      Log.infoCont [exon|📦 ##{dep}|]

reportFailed ::
  RenderMutation a =>
  NonEmpty (DepMutation a) ->
  M ()
reportFailed failed =
  for_ failed \ mut -> Log.infoCont (listFailed mut)

-- TODO print unmodified deps, maybe only if none succeeded otherwise
--
-- TODO don't print the env name when using @sets = "all"@ (or even when only one env is processed?)
envMutations ::
  RenderMutation a =>
  EnvName ->
  BuildResult a ->
  M ()
envMutations env = \case
  NoActionRequired ->
    reportHeadline "No action required."
  FatalBuildFailure message -> do
    reportHeadline "Fatal error:"
    Log.error message
  AllBuildsSucceeded changes -> do
    reportHeadline "All dependencies were processed successfully."
    reportSuccesses changes
    traverse_ reportRemovable changes.removable
  SomeBuildsFailed {changes, failed} -> do
    reportHeadline "Some dependencies failed:"
    reportFailed failed
    reportSuccesses changes
    traverse_ reportRemovable changes.removable
  where
    reportHeadline :: Text -> M ()
    reportHeadline status = Log.info [exon|Result for '##{env}': #{status}|]

mutations ::
  RenderMutation a =>
  BuildResults a ->
  M ()
mutations results =
  traverse_ (uncurry envMutations) (Map.toList results.envs)

handlersProd ::
  RenderMutation a =>
  ReportHandlers a
handlersProd =
  ReportHandlers {mutations}
