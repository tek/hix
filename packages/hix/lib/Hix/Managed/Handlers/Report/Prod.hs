module Hix.Managed.Handlers.Report.Prod where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Exon (exon)

import qualified Hix.Data.Bounds
import Hix.Data.Bounds (UninitializedBounds)
import Hix.Data.Monad (M)
import Hix.Data.Package (PackageName)
import qualified Hix.Data.Version
import Hix.Data.Version (NewVersion (NewVersion), renderNewRange)
import qualified Hix.Log as Log
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (Candidate (Candidate), RenderMutation (renderMutation))
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (BuildResult)
import qualified Hix.Managed.Handlers.Report
import Hix.Managed.Handlers.Report (ReportHandlers (ReportHandlers))
import Hix.Pretty (showP)

mutations ::
  RenderMutation a =>
  UninitializedBounds ->
  BuildResult a ->
  M ()
mutations uninitialized results = do
  for_ (nonEmpty results.success) \ success -> do
    Log.info "Updated dependency versions:"
    for_ success \ p ->
      Log.infoCont (listSuccess p)
  for_ (nonEmpty results.failed) \ failed -> do
    Log.info "Failed to find working versions for some dependencies:"
    for_ failed \ mut ->
      Log.infoCont (listFailed mut)
  for_ (Map.toList uninitialized.deps) \ (target, deps) -> do
    for_ (nonEmpty (toList (Set.intersection deps successNames))) \ added -> do
      Log.info [exon|You can remove the #{showP uninitialized.targetBound} bounds from these deps of '##{target}':|]
      for_ added \ dep ->
        Log.infoCont [exon|📦 ##{dep}|]
  where
    listSuccess Candidate {version = NewVersion {package, version}, range} =
      listPkg package (showP version) range

    listFailed mutation =
      [exon|📦 #{renderMutation mutation}|]

    listPkg (package :: PackageName) version range =
      [exon|📦 ##{package} #{version} [#{renderNewRange range}]|]

    successNames = Set.fromList (results.success <&> (.version.package))

handlersProd ::
  RenderMutation a =>
  ReportHandlers a
handlersProd =
  ReportHandlers {mutations}
