module Hix.Managed.ReleaseMaintenance where

import Distribution.Pretty (Pretty (pretty))
import Exon (exon)

import Hix.Class.Map (nTo, nViaA)
import qualified Hix.Color as Color
import Hix.Data.Monad (AppResources (..), M, appRes)
import Hix.Data.Options (ManagedOptions (..), ProjectOptions (..), ReleaseMaintOptions (..), RevisionOptions (..))
import Hix.Data.PackageName (LocalPackage (..))
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Config (cabalConfig)
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.MaintContext (MaintContextProto (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.RevisionConfig (RevisionConfig (..))
import Hix.Managed.Data.SpecialMaintHandlers (SpecialMaintHandlers (..))
import Hix.Managed.Git (runGitApi)
import Hix.Managed.Handlers.Context (ContextKey (ContextMaint), jsonOrQueryProd)
import Hix.Managed.Handlers.Maint (MaintHandlers (..))
import qualified Hix.Managed.Handlers.Maint.Prod as Maint
import qualified Hix.Managed.Handlers.Maint.Test as Maint
import Hix.Managed.Handlers.Report.Prod (blankLine)
import Hix.Managed.Handlers.Revision (RevisionHandlers (..))
import qualified Hix.Managed.Handlers.Revision.Prod as Revision
import Hix.Managed.Maint.Data.MaintPlan (MaintPlan (..))
import Hix.Managed.Maint.Data.MaintResult (MaintResult (..))
import Hix.Managed.Maint.Git (GitMaint (..), GitRevision (..))
import Hix.Managed.Maint.MaintPlan (maintPlan)
import Hix.Managed.Maint.MaintResult (outputResults)
import Hix.Managed.Maint.Package (maintPackage, publishRevisionIfChanged)
import Hix.Managed.Maint.RevisionPlan (revisionPlan)
import qualified Hix.Managed.MaintContext as MaintContext
import Hix.Monad (appContext, ask)
import Hix.Pretty (showP, showPL)

packageReport :: LocalPackage -> MaintResult -> M ()
packageReport (LocalPackage name) result = do
  Log.info (Color.package name)
  Log.infoP (pretty result)

report :: Packages MaintResult -> M ()
report packages = do
  blankLine
  sequence_ (intersperse blankLine (nTo packages packageReport))

releaseMaintenance ::
  MaintHandlers ->
  MaintConfig ->
  MaintContextProto ->
  M (Packages MaintResult)
releaseMaintenance handlers config proto =
  appContext "performing release maintenance" do
    root <- appRes.root
    context <- MaintContext.validate proto
    plan <- maintPlan context config.targets
    Log.trace [exon|Maintenance plan: #{showP plan}|]
    results <- runGitApi handlers.git root "release maintenance" \ git@GitMaint {bracket} -> bracket do
      tags <- git.readTags
      Log.trace [exon|Found tags: #{showPL tags}|]
      nViaA (traverse (maintPackage handlers config git tags)) plan.targets
    AppResources {output = format, target} <- ask
    report results
    outputResults target format results
    pure results

releaseMaintenanceCli :: ReleaseMaintOptions -> M ()
releaseMaintenanceCli options = do
  context <- jsonOrQueryProd ContextMaint options.context
  cabal <- cabalConfig context.hackage options.managed.project.cabal
  handlers <- case options.handlers of
    Just MaintHandlersTestMaint -> Maint.handlersTest options.managed options.config cabal
    Nothing -> Maint.handlersProd options.managed options.config cabal
  void $ releaseMaintenance handlers options.config context

publishRevisions :: RevisionHandlers -> RevisionConfig -> MaintContextProto -> M ()
publishRevisions handlers config proto =
  appContext "processing pending revisions" do
    root <- appRes.root
    context <- MaintContext.validate proto
    runGitApi handlers.git root "revisions" \ git@GitRevision {bracket} -> bracket do
      revisionPlan git context config.targets >>= \case
        Nothing -> Log.info "No packages are eligible for publishing a revision."
        Just targets -> traverse_ (publishRevisionIfChanged handlers) targets

revisionCli :: RevisionOptions -> M ()
revisionCli options = do
  context <- jsonOrQueryProd ContextMaint options.context
  cabal <- cabalConfig context.hackage options.cabal
  handlers <- Revision.handlersProd options.config cabal
  publishRevisions handlers options.config context
