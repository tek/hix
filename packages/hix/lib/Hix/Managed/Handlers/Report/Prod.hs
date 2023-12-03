module Hix.Managed.Handlers.Report.Prod where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Exon (exon)

import qualified Hix.Data.Bounds
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.Package (PackageName)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion), renderNewRange)
import qualified Hix.Log as Log
import Hix.Managed.Build.Mutation (RenderMutation (renderMutation))
import qualified Hix.Managed.Data.Build as BuildResults
import Hix.Managed.Data.Build (BuildResult, BuildResults)
import qualified Hix.Managed.Data.Candidate
import Hix.Managed.Data.Candidate (Candidate (Candidate))
import qualified Hix.Managed.Handlers.Report
import Hix.Managed.Handlers.Report (ReportHandlers (ReportHandlers))
import Hix.Pretty (showP)

-- TODO print unmodified deps, maybe only if none succeeded otherwise
envMutations ::
  RenderMutation a =>
  EnvName ->
  BuildResult a ->
  M ()
envMutations env result = do
  Log.info [exon|Processed environment '##{env}':|]
  for_ (nonEmpty changed) \ success -> do
    Log.info "Updated dependency versions:"
    for_ (NonEmpty.reverse success) \ p ->
      Log.infoCont (listSuccess p)
  for_ (nonEmpty result.failed) \ failed -> do
    Log.info "Failed to find working versions for some dependencies:"
    for_ (NonEmpty.reverse failed) \ mut ->
      Log.infoCont (listFailed mut)
  for_ (Map.toList result.removable.deps) \ (target, deps) -> do
      Log.info [exon|You can remove the #{showP result.removable.targetBound} bounds from these deps of '##{target}':|]
      for_ deps \ dep ->
        Log.infoCont [exon|📦 ##{dep}|]
  where
    listSuccess Candidate {version = NewVersion {package, version}, range} =
      listPkg package (showP version) range

    listFailed mutation =
      [exon|📦 #{renderMutation mutation}|]

    listPkg (package :: PackageName) version range =
      [exon|📦 ##{package} #{version} [#{renderNewRange range}]|]

    changed = BuildResults.changed result

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
