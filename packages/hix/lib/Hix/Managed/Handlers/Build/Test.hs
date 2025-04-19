module Hix.Managed.Handlers.Build.Test where

import Data.IORef (IORef)
import Data.Map.Strict ((!?))

import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Data.Overrides (Overrides)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Data.Config (CabalConfig)
import qualified Hix.Managed.Cabal.Data.Packages
import Hix.Managed.Cabal.Data.Packages (GhcPackages)
import Hix.Managed.Data.BuildConfig (BuildConfig, SpecialBuildHandlers (..))
import Hix.Managed.Data.Mutation (FailedMutation)
import Hix.Managed.Data.StageState (BuildResult)
import qualified Hix.Managed.Handlers.AvailableVersions as AvailableVersions
import qualified Hix.Managed.Handlers.AvailableVersions.Test as AvailableVersions
import Hix.Managed.Handlers.Build (BuildHandlers (..), versionsBuilder)
import Hix.Managed.Handlers.Build.Prod (handlersProd, initCabalProd)
import Hix.Managed.Handlers.Cabal.Prod (testPackagesBump, testPackagesMaint)
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import qualified Hix.Managed.Handlers.Report.Test as ReportHandlers
import qualified Hix.Managed.Handlers.SourceHash as SourceHashHandlers
import qualified Hix.Managed.Handlers.StateFile.Test as StateFile

handlersUnitTest ::
  MonadIO m =>
  GhcPackages ->
  (Versions -> M (BuildResult, Overrides)) ->
  m (BuildHandlers, IORef [Expr], IORef [FailedMutation])
handlersUnitTest ghcPackages builder = do
  (stateFile, stateFileRef) <- StateFile.handlersUnitTest
  (report, mutationsRef) <- ReportHandlers.handlersUnitTest
  let
    handlers =
      BuildHandlers {
        project = ProjectHandlers {
          stateFile,
          report
        },
        cabal = initCabalProd def,
        withBuilder = versionsBuilder SourceHashHandlers.handlersNull builder,
        versions = AvailableVersions.handlersTest ghcPackages.available
      }
  pure (handlers, stateFileRef, mutationsRef)

handlersTest ::
  Map PackageName PackageId ->
  ProjectHandlers ->
  BuildConfig ->
  CabalConfig ->
  M BuildHandlers
handlersTest packages project buildConf cabalConf = do
  handlers <- handlersProd project buildConf cabalConf
  pure handlers {
    cabal = initCabalProd cabalConf,
    versions = AvailableVersions.handlersActionOne \ name -> pure ((.version) <$> packages !? name)
  }

handlersBumpTest ::
  ProjectHandlers ->
  BuildConfig ->
  CabalConfig ->
  M BuildHandlers
handlersBumpTest =
  handlersTest testPackagesBump

chooseHandlers ::
  Maybe SpecialBuildHandlers ->
  ProjectHandlers ->
  BuildConfig ->
  CabalConfig ->
  M BuildHandlers
chooseHandlers = \case
  Just BuildHandlersTestBump -> handlersBumpTest
  Just BuildHandlersTestMaint -> handlersTest testPackagesMaint
  Nothing -> handlersProd
