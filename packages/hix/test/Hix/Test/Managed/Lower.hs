module Hix.Test.Managed.Lower where

import Data.IORef (readIORef)
import Hedgehog (TestT, evalEither, evalMaybe)

import Hix.Class.Map (nKeys)
import Hix.Data.Monad (M)
import Hix.Data.NixExpr (Expr)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Data.Config (GhcDb (GhcDbSynthetic))
import Hix.Managed.Cabal.Data.Packages (GhcPackages)
import qualified Hix.Managed.Data.EnvConfig
import Hix.Managed.Data.EnvConfig (EnvConfig (EnvConfig))
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectContext (ProjectContext)
import qualified Hix.Managed.Data.ProjectContextProto
import Hix.Managed.Data.ProjectContextProto (ProjectContextProto (ProjectContextProto))
import Hix.Managed.Data.ProjectResult (ProjectResult)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Managed.Data.StageState (BuildStatus)
import Hix.Managed.Handlers.Lower (LowerHandlers (..))
import qualified Hix.Managed.Handlers.Lower.Test as LowerHandlers
import Hix.Managed.ProjectContext (withProjectContext)
import Hix.Test.Utils (runMTest)

lowerTest ::
  HasCallStack =>
  Bool ->
  Packages ManagedPackageProto ->
  GhcPackages ->
  ProjectStateProto ->
  (Versions -> M BuildStatus) ->
  (LowerHandlers -> ProjectContext -> M ProjectResult) ->
  TestT IO Expr
lowerTest debug packages ghcPackages initialState buildVersions main =
  withFrozenCallStack do
    (handlers, stateFileRef, _) <- LowerHandlers.handlersUnitTest buildVersions ghcPackages
    let
      proto =
        ProjectContextProto {
          packages,
          state = initialState,
          envs = [("lower", EnvConfig {targets = nKeys packages, ghc = GhcDbSynthetic ghcPackages})],
          buildOutputsPrefix = Nothing
        }
    result <- liftIO do
      runMTest debug $ withProjectContext handlers.build def proto (main handlers)
    evalEither result
    evalMaybe . head =<< liftIO (readIORef stateFileRef)
