module Hix.Bump where

import Control.Monad (foldM)
import Control.Monad.Extra (mapMaybeM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Distribution.Pretty (pretty)
import Distribution.Version (versionNumbers)
import Exon (exon)

import Hix.Bump.Build (buildWithBumps, renderDeps)
import Hix.Bump.Handlers.Test (testHandlers)
import Hix.Bump.Handlers.Prod (prodHandlers, rootOrCwd)
import Hix.Bump.Version (bumpPackages)
import qualified Hix.Console as Console
import qualified Hix.Data.BumpConfig
import Hix.Data.BumpConfig (BumpConfig, BumpEnv, ManagedConfig (deps))
import qualified Hix.Data.BumpHandlers
import Hix.Data.BumpHandlers (BuildsState (BuildsState), BumpHandlers, SpecialBumpHandlers (TestBumpHandlers))
import Hix.Data.ComponentConfig (PackageDeps, PackageName)
import Hix.Data.Error (Error (BumpError))
import qualified Hix.Data.Version
import Hix.Data.Version (NewRange (NewRange), VersionBump (VersionBump))
import Hix.Json (jsonConfigE)
import Hix.Monad (M, throwM)
import qualified Hix.Options
import Hix.Options (BumpOptions)

printBump :: VersionBump -> M ()
printBump VersionBump {package, range, newVersion} =
  liftIO do
    Console.info [exon|  New version for '##{package}': #{versionString}|]
    for_ range \ (NewRange r) ->
      Console.info [exon|    New range: #{show (pretty r)}|]
  where
    versionString = Text.intercalate "." (show <$> versionNumbers newVersion)

depsParseError :: String -> M a
depsParseError err =
  throwM (BumpError [exon|Invalid json for packages: #{toText err}|])

finalManaged ::
  BumpConfig ->
  ManagedConfig ->
  ManagedConfig ->
  ManagedConfig
finalManaged conf old new
  | conf.latestOverrides = new
  | otherwise = old {deps = new.deps}

updateProject ::
  BumpHandlers ->
  BumpConfig ->
  ManagedConfig ->
  BuildsState ->
  M ()
updateProject handlers conf originalManaged state = do
  root <- rootOrCwd conf.projectRoot
  depsFile <- handlers.initDepsFile root conf.file
  handlers.writeDepsFile depsFile (renderDeps (finalManaged conf originalManaged state.managed))
  traverse_ (uncurry handlers.reportBumps) (Map.toList state.results)

buildPackages ::
  BumpHandlers ->
  BumpConfig ->
  ManagedConfig ->
  [(PackageName, (PackageDeps, NonEmpty VersionBump))] ->
  M BuildsState
buildPackages handlers conf originalManaged =
  foldM step (BuildsState mempty originalManaged)
  where
    step z (pkg, (pdeps, bumps)) = do
      (result, managed) <- buildWithBumps handlers conf z.managed pkg pdeps bumps
      pure BuildsState {results = Map.insert pkg result z.results, managed}

buildAndUpdate ::
  BumpHandlers ->
  BumpConfig ->
  ManagedConfig ->
  [(PackageName, (PackageDeps, NonEmpty VersionBump))] ->
  M ()
buildAndUpdate handlers conf managed bumps = do
  result <- buildPackages handlers conf managed bumps
  updateProject handlers conf managed result

reportOrUpdate ::
  BumpHandlers ->
  BumpConfig ->
  ManagedConfig ->
  [(PackageName, (PackageDeps, NonEmpty VersionBump))] ->
  Bool ->
  M ()
reportOrUpdate handlers conf managed bumps = \case
  True ->
    buildAndUpdate handlers conf managed bumps
  False ->
    for_ bumps \case
      (pkg, (_, bs)) -> do
        Console.info [exon|For dependencies of '##{pkg}':|]
        traverse_ printBump bs

reportUpToDate ::
  [(PackageName, Maybe (PackageDeps, NonEmpty VersionBump))] ->
  M [(PackageName, (PackageDeps, NonEmpty VersionBump))]
reportUpToDate =
  mapMaybeM \case
    (pkg, Nothing) -> do
      Console.info [exon|All dependencies of '##{pkg}' are up to date.|]
      pure Nothing
    (pkg, Just bumps) ->
      pure (Just (pkg, bumps))

bump ::
  BumpHandlers ->
  BumpEnv ->
  BumpConfig ->
  M ()
bump handlers env conf = do
  results <- bumpPackages handlers env.deps env.managed conf.env packages
  actionable <- reportUpToDate results
  reportOrUpdate handlers conf env.managed actionable conf.updateProject
  where
    packages = maybe allPackages pure conf.package

    allPackages = Map.keys env.deps

chooseHandlers :: Maybe SpecialBumpHandlers -> IO BumpHandlers
chooseHandlers = \case
  Just TestBumpHandlers -> testHandlers
  Nothing -> prodHandlers

bumpCli :: BumpOptions -> M ()
bumpCli opts = do
  env <- jsonConfigE BumpError opts.env
  handlers <- liftIO (chooseHandlers opts.handlers)
  bump handlers env opts.config
