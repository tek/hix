module Hix.Managed.Handlers.Lower.Test where

import Data.IORef (IORef)

import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Data.Version (Versions)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages)
import Hix.Managed.Cabal.Mock.SourcePackage (queryVersions, sourcePackageVersions)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Mutation (FailedMutation)
import Hix.Managed.Data.StageState (BuildStatus)
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Handlers.Build (BuildOutputsPrefix)
import qualified Hix.Managed.Handlers.Build.Test as BuildHandlers
import Hix.Managed.Handlers.Lower (LowerHandlers (..), handlersNull)
import Hix.Managed.Handlers.Lower.Prod (handlersProd)

handlersTest ::
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  Bool ->
  M LowerHandlers
handlersTest =
  handlersProd

handlersUnitTest ::
  MonadIO m =>
  (Versions -> M BuildStatus) ->
  GhcPackages ->
  m (LowerHandlers, IORef [Expr], IORef [FailedMutation])
handlersUnitTest buildVersions ghcPackages = do
  (build, stateFileRef, mutationsRef) <- BuildHandlers.handlersUnitTest buildVersions
  pure (handlersNull {build, versions}, stateFileRef, mutationsRef)
  where
    versions = queryVersions (sourcePackageVersions ghcPackages.available)
