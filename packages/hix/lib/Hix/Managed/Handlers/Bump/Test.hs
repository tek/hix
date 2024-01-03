module Hix.Managed.Handlers.Bump.Test where

import Data.IORef (IORef)
import Data.Map.Strict ((!?))
import Distribution.Version (Version)
import Exon (exon)

import Hix.Data.Error (Error (Client))
import Hix.Data.NixExpr (Expr)
import qualified Hix.Data.PackageId
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Versions)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages)
import Hix.Managed.Cabal.Mock.SourcePackage (queryVersionsLatest, sourcePackageVersions)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Mutation (FailedMutation)
import Hix.Managed.Data.StageState (BuildStatus)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildOutputsPrefix)
import qualified Hix.Managed.Handlers.Build.Test as Build
import Hix.Managed.Handlers.Bump (BumpHandlers (..), handlersNull)
import Hix.Managed.Handlers.Cabal.Prod (testPackages)
import Hix.Monad (M, throwM)

latestVersionNixTest :: PackageName -> M (Maybe Version)
latestVersionNixTest name =
  maybe invalid found (testPackages !? name)
  where
    invalid = throwM (Client [exon|Invalid package for latestVersion: ##{name}|])
    found = pure . Just . (.version)

handlersTest ::
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  IO BumpHandlers
handlersTest stateFileConf envsConf buildOutputsPrefix oldest = do
  build <- Build.handlersTest stateFileConf envsConf buildOutputsPrefix oldest
  pure BumpHandlers {
    build,
    latestVersion = latestVersionNixTest
  }

-- TODO unify with Lower
handlersUnitTest ::
  MonadIO m =>
  (Versions -> M BuildStatus) ->
  GhcPackages ->
  m (BumpHandlers, IORef [Expr], IORef [FailedMutation])
handlersUnitTest buildVersions ghcPackages = do
  (build, stateFileRef, mutationsRef) <- Build.handlersUnitTest buildVersions
  pure (handlersNull {build, latestVersion}, stateFileRef, mutationsRef)
  where
    latestVersion =
      queryVersionsLatest (sourcePackageVersions ghcPackages.available)
