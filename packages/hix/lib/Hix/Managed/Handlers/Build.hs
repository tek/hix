module Hix.Managed.Handlers.Build where

import Data.IORef (IORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Cabal.Data.Config (GhcDb)
import Hix.Managed.Data.Constraints (EnvConstraints)
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.StageState (BuildFailure (UnknownFailure), BuildResult (BuildFailure))
import qualified Hix.Managed.Handlers.AvailableVersions as AvailableVersions
import Hix.Managed.Handlers.AvailableVersions (AvailableVersionsHandlers)
import qualified Hix.Managed.Handlers.Cabal as Solve
import qualified Hix.Managed.Handlers.Cabal as Cabal
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import qualified Hix.Managed.Handlers.Project as Project
import Hix.Managed.Handlers.Project (ProjectHandlers)
import Hix.Managed.Handlers.SourceHash (SourceHashHandlers)
import Hix.Managed.Overrides (packageOverrides)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)

data EnvBuilder =
  EnvBuilder {
    cabal :: CabalHandlers,
    buildWithState :: Bool -> Versions -> [PackageId] -> M (BuildResult, (Overrides, Set PackageId))
  }

data Builder =
  Builder {
    withEnvBuilder :: ∀ a . CabalHandlers -> EnvContext -> Initial EnvState -> (EnvBuilder -> M a) -> M a
  }

runBuilder :: Builder -> CabalHandlers -> EnvContext -> Initial EnvState -> (EnvBuilder -> M a) -> M a
runBuilder Builder {withEnvBuilder} = withEnvBuilder

data BuildHandlers =
  BuildHandlers {
    project :: ProjectHandlers,
    cabal :: Packages ManagedPackage -> GhcDb -> M CabalHandlers,
    withBuilder :: ∀ a . (Builder -> M a) -> M a,
    versions :: AvailableVersionsHandlers
  }

testBuilder ::
  (Bool -> Versions -> [PackageId] -> M (BuildResult, (Overrides, Set PackageId))) ->
  (Builder -> M a) ->
  M a
testBuilder buildWithState use =
  use Builder {withEnvBuilder = \ cabal _ _ useE -> useE EnvBuilder {cabal, buildWithState}}

versionsBuilder :: SourceHashHandlers -> (Versions -> M BuildResult) -> (Builder -> M a) -> M a
versionsBuilder hackage build =
  testBuilder \ _ versions overrideVersions -> do
    overrides <- packageOverrides hackage overrideVersions
    status <- build versions
    pure (status, (overrides, mempty))

handlersNull :: BuildHandlers
handlersNull =
  BuildHandlers {
    project = Project.handlersNull,
    cabal = \ _ _ -> pure Solve.handlersNull,
    withBuilder = testBuilder \ _ _ _ -> pure (BuildFailure UnknownFailure, mempty),
    versions = AvailableVersions.handlersNull
  }

wrapCabal :: (CabalHandlers -> CabalHandlers) -> BuildHandlers -> BuildHandlers
wrapCabal f BuildHandlers {..} =
  BuildHandlers {cabal = \ p d -> f <$> cabal p d, ..}

logCabal ::
  MonadIO m =>
  BuildHandlers ->
  m (IORef [(EnvConstraints, Maybe SolverPlan)], BuildHandlers)
logCabal handlers = do
  ref <- liftIO (newIORef [])
  pure (ref, wrapCabal (Cabal.logCabal ref) handlers)
