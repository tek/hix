module Hix.Managed.Lower.App where

import Distribution.Pretty (Pretty)
import Distribution.Version (Version)
import Exon (exon)
import Path (Abs, Dir, Path)

import Hix.Data.Dep (Dep)
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
import Hix.Managed.App (ManagedApp, managedApp, processAppResult, runManagedApp)
import Hix.Managed.Build (buildMutations)
import Hix.Managed.Build.Mutation (DepMutation)
import Hix.Managed.Data.Build (BuildResult)
import qualified Hix.Managed.Data.ManagedConfig
import qualified Hix.Managed.Data.SolverBounds as SolverBounds
import Hix.Managed.Data.SolverBounds (SolverBounds, noBounds, optimizeBounds)
import qualified Hix.Managed.Handlers.LowerInit
import Hix.Managed.Handlers.LowerInit (LowerInitHandlers, SpecialLowerInitHandlers (TestLowerInitHandlers))
import qualified Hix.Managed.Handlers.LowerInit.Prod as LowerInit
import qualified Hix.Managed.Handlers.LowerInit.Test as LowerInit
import qualified Hix.Managed.Handlers.LowerOptimize
import Hix.Managed.Handlers.LowerOptimize (
  LowerOptimizeHandlers,
  SpecialLowerOptimizeHandlers (TestLowerOptimizeHandlers),
  )
import qualified Hix.Managed.Handlers.LowerOptimize.Prod as LowerOptimize
import qualified Hix.Managed.Handlers.LowerOptimize.Test as LowerOptimize
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
  MutationHandlers a s ->
  ManagedApp ->
  M (BuildResult a)
lowerCommon initialBounds consState candidates mutationHandlers app = do
  mutations <- catMaybes <$> traverse candidates app.deps
  buildMutations app.build mutationHandlers app.conf app.job app.state mutations ext
  where
    solverBounds = SolverBounds.fromConfig app.solverBounds <> initialBounds
    ext = consState solverBounds

lowerInit ::
  LowerInitHandlers ->
  LowerInitConfig ->
  ManagedApp ->
  M (BuildResult LowerInit)
lowerInit handlers LowerInitConfig {initialBounds, lowerMajor} app =
  lowerCommon (initialBounds <> noBounds app.deps) LowerInitState candidates mutationHandlers app
  where
    candidates = candidatesInit handlers.versions lowerMajor
    mutationHandlers = Mutation.handlersLowerInit app.build

chooseHandlersInit ::
  Maybe (Path Abs Dir) ->
  Bool ->
  Maybe SpecialLowerInitHandlers ->
  M LowerInitHandlers
chooseHandlersInit ghc oldest = \case
  Just TestLowerInitHandlers -> LowerInit.handlersTest ghc oldest
  Nothing -> LowerInit.handlersProd ghc oldest

lowerInitCli :: LowerInitOptions -> M ()
lowerInitCli opts = do
  env <- jsonConfigE Client opts.env
  handlers <- chooseHandlersInit opts.config.ghc opts.lowerInit.oldest opts.handlers
  runManagedApp handlers.build handlers.report env opts.config \ app ->
    Right <$> lowerInit handlers opts.lowerInit app

lowerOptimize ::
  LowerOptimizeHandlers ->
  LowerOptimizeConfig ->
  ManagedApp ->
  M (BuildResult LowerOptimize)
lowerOptimize handlers LowerOptimizeConfig {initialBounds} app =
  lowerCommon (initialBounds <> optimizeBounds app.deps) LowerOptimizeState candidates mutationHandlers app
  where
    candidates = candidatesOptimize handlers.versions
    mutationHandlers = Mutation.handlersLowerOptimize handlers.build

chooseHandlersOptimize ::
  Maybe (Path Abs Dir) ->
  Bool ->
  Maybe SpecialLowerOptimizeHandlers ->
  M LowerOptimizeHandlers
chooseHandlersOptimize ghc oldest = \case
  Just TestLowerOptimizeHandlers -> LowerOptimize.handlersTest ghc oldest
  Nothing -> LowerOptimize.handlersProd ghc oldest

lowerOptimizeCli :: LowerOptimizeOptions -> M ()
lowerOptimizeCli opts = do
  env <- jsonConfigE Client opts.env
  handlers <- chooseHandlersOptimize opts.config.ghc opts.lowerOptimize.oldest opts.handlers
  managedApp handlers.build env opts.config \ app -> do
    result <- lowerOptimize handlers opts.lowerOptimize app
    processAppResult handlers.build handlers.report env opts.config app (Right result)
