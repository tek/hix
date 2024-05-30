module Hix.Managed.Handlers.Build.Test where

import Data.IORef (IORef)
import Data.Map.Strict ((!?))
import Exon (exon)

import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import qualified Hix.Data.PackageId
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version, Versions)
import Hix.Managed.Cabal.Data.Config (CabalConfig)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages)
import Hix.Managed.Cabal.Mock.SourcePackage (queryVersions, queryVersionsLatest, sourcePackageVersions)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Mutation (FailedMutation)
import Hix.Managed.Data.StageState (BuildResult)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (
  BuildHandlers (..),
  BuildOutputsPrefix,
  SpecialBuildHandlers (TestBumpHandlers),
  versionsBuilder,
  )
import Hix.Managed.Handlers.Build.Prod (handlersProd)
import qualified Hix.Managed.Handlers.Cabal.Prod as CabalHandlers
import Hix.Managed.Handlers.Cabal.Prod (testPackagesBump)
import qualified Hix.Managed.Handlers.Hackage as HackageHandlers
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers
import qualified Hix.Managed.Handlers.StateFile.Test as StateFile
import Hix.Monad (clientError)

handlersUnitTest ::
  MonadIO m =>
  GhcPackages ->
  (Versions -> M BuildResult) ->
  m (BuildHandlers, IORef [Expr], IORef [FailedMutation])
handlersUnitTest ghcPackages builder = do
  (stateFile, stateFileRef) <- StateFile.handlersUnitTest
  (report, mutationsRef) <- ReportHandlers.handlersUnitTest
  let
    handlers =
      BuildHandlers {
        stateFile,
        report,
        cabal,
        withBuilder = versionsBuilder HackageHandlers.handlersNull builder,
        versions = queryVersions versions,
        latestVersion = queryVersionsLatest versions
      }
  pure (handlers, stateFileRef, mutationsRef)
  where
    cabal = CabalHandlers.handlersProd def False
    versions = sourcePackageVersions ghcPackages.available

latestVersionNixTestBump :: PackageName -> M (Maybe Version)
latestVersionNixTestBump name =
  maybe invalid found (testPackagesBump !? name)
  where
    invalid = clientError [exon|Invalid package for latestVersion: ##{name}|]
    found = pure . Just . (.version)

handlersBumpTest ::
  MonadIO m =>
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  BuildConfig ->
  CabalConfig ->
  Bool ->
  m BuildHandlers
handlersBumpTest stateFileConf envsConf buildOutputsPrefix buildConf cabalConf oldest = do
  handlers <- handlersProd stateFileConf envsConf buildOutputsPrefix buildConf cabalConf oldest
  pure handlers {
    cabal = CabalHandlers.handlersTest cabalConf oldest,
    latestVersion = latestVersionNixTestBump
  }

chooseHandlers ::
  MonadIO m =>
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  BuildConfig ->
  CabalConfig ->
  Maybe SpecialBuildHandlers ->
  m BuildHandlers
chooseHandlers stateFileConf envsConf buildOutputsPrefix buildConf cabalConf = \case
  Just TestBumpHandlers -> handlersBumpTest stateFileConf envsConf buildOutputsPrefix buildConf cabalConf oldest
  Nothing -> handlersProd stateFileConf envsConf buildOutputsPrefix buildConf cabalConf oldest
  where
    oldest = False
