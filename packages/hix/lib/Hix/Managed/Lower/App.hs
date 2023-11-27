module Hix.Managed.Lower.App where

import Distribution.Pretty (Pretty)
import Distribution.Version (Version)
import Exon (exon)

import Hix.Data.Dep (Dep)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Client))
import qualified Hix.Data.LowerConfig
import Hix.Data.LowerConfig (LowerInitConfig (LowerInitConfig), LowerOptimizeConfig (LowerOptimizeConfig))
import Hix.Data.Monad (M)
import qualified Hix.Data.Options
import Hix.Data.Options (LowerInitOptions, LowerOptimizeOptions)
import Hix.Data.Overrides (Override (..))
import Hix.Data.Package (PackageName)
import Hix.Json (jsonConfigE)
import qualified Hix.Log as Log
import qualified Hix.Managed.App
import Hix.Managed.App (ManagedApp, runManagedApp)
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Mutation (DepMutation, RenderMutation)
import Hix.Managed.Data.Build (BuildResult)
import qualified Hix.Managed.Data.ManagedJob
import qualified Hix.Managed.Data.SolverBounds as SolverBounds
import Hix.Managed.Data.SolverBounds (SolverBounds, noBounds, optimizeBounds)
import qualified Hix.Managed.Handlers.Build
import qualified Hix.Managed.Handlers.Lower
import Hix.Managed.Handlers.Lower (LowerHandlers, SpecialLowerHandlers (TestLowerHandlers))
import qualified Hix.Managed.Handlers.Lower.Prod as Lower
import qualified Hix.Managed.Handlers.Lower.Test as Lower
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import qualified Hix.Managed.Handlers.Mutation.LowerInit as Mutation
import qualified Hix.Managed.Handlers.Mutation.LowerOptimize as Mutation
import Hix.Managed.Lower.Candidates (candidatesInit, candidatesOptimize)
import Hix.Managed.Lower.Data.LowerInit (LowerInit, LowerInitState (LowerInitState))
import Hix.Managed.Lower.Data.LowerOptimize (LowerOptimize, LowerOptimizeState (LowerOptimizeState))
import Hix.Pretty (showP)

listPackageVersion :: PackageName -> Version -> Text
listPackageVersion pkg version =
  [exon|📦 ##{pkg} #{showP version}|]

reportOverrides :: [(PackageName, Override)] -> M ()
reportOverrides [] =
  Log.info "No versions for lower bounds could be determined."
reportOverrides versions = do
  Log.info "Wrote lower bound overrides:"
  for_ versions \ (pkg, Override {version}) ->
    Log.infoCont (listPackageVersion pkg version)

reportFailures :: NonEmpty (PackageName, Version) -> M ()
reportFailures pkgs = do
  Log.warn "The build failed with some of the lower bound versions:"
  for_ pkgs \ (pkg, version) -> Log.infoCont (listPackageVersion pkg version)

lowerCommon ::
  Pretty a =>
  SolverBounds ->
  (SolverBounds -> s) ->
  (Dep -> M (Maybe (DepMutation a))) ->
  (EnvName -> M (MutationHandlers a s)) ->
  ManagedApp ->
  M (BuildResult a)
lowerCommon initialBounds consState candidates mkMutationHandlers app = do
  mutations <- catMaybes <$> traverse candidates app.deps
  mutationHandlers <- mkMutationHandlers app.job.env
  buildMutations app.build mutationHandlers app.conf app.job app.state mutations ext
  where
    solverBounds = SolverBounds.fromConfig app.solverBounds <> initialBounds
    ext = consState solverBounds

lowerInit ::
  LowerHandlers LowerInit ->
  LowerInitConfig ->
  ManagedApp ->
  M (BuildResult LowerInit)
lowerInit handlers LowerInitConfig {initialBounds, lowerMajor} app =
  lowerCommon (initialBounds <> noBounds app.deps) LowerInitState candidates mutationHandlers app
  where
    candidates = candidatesInit handlers.versions lowerMajor
    mutationHandlers = Mutation.handlersLowerInit app.build.hackage handlers.solve

chooseHandlers ::
  RenderMutation a =>
  Bool ->
  Maybe SpecialLowerHandlers ->
  M (LowerHandlers a)
chooseHandlers oldest = \case
  Just TestLowerHandlers -> Lower.handlersTest oldest
  Nothing -> Lower.handlersProd oldest

lowerInitCli :: LowerInitOptions -> M ()
lowerInitCli opts = do
  env <- jsonConfigE Client opts.env
  handlers <- chooseHandlers opts.lowerInit.oldest opts.handlers
  runManagedApp handlers.build handlers.report env opts.config \ app ->
    Right <$> lowerInit handlers opts.lowerInit app

lowerOptimize ::
  LowerHandlers LowerOptimize ->
  LowerOptimizeConfig ->
  ManagedApp ->
  M (BuildResult LowerOptimize)
lowerOptimize handlers LowerOptimizeConfig {initialBounds} app =
  lowerCommon (initialBounds <> optimizeBounds app.deps) LowerOptimizeState candidates mutationHandlers app
  where
    candidates = candidatesOptimize handlers.versions
    mutationHandlers = Mutation.handlersLowerOptimize app.build.hackage handlers.solve

lowerOptimizeCli :: LowerOptimizeOptions -> M ()
lowerOptimizeCli opts = do
  env <- jsonConfigE Client opts.env
  handlers <- chooseHandlers opts.lowerOptimize.oldest opts.handlers
  runManagedApp handlers.build handlers.report env opts.config \ app ->
    Right <$> lowerOptimize handlers opts.lowerOptimize app
