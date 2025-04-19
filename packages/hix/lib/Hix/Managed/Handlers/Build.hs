module Hix.Managed.Handlers.Build where

import Data.IORef (IORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Cabal.Data.Config (GhcDb (..))
import Hix.Managed.Cabal.Data.InstalledOverrides (InstalledOverrides)
import Hix.Managed.Cabal.Data.SolvedId (SolvedId)
import Hix.Managed.Data.Constraints (EnvConstraints)
import Hix.Managed.Data.EnvContext (EnvContext (..))
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial, initial)
import Hix.Managed.Data.ManagedPackage (ProjectPackages)
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
import Hix.Monad (noteClient)

newtype InitCabal =
  InitCabal { run :: InstalledOverrides -> GhcDb -> M (CabalHandlers, Set LocalPackage) }

data EnvBuilderContext =
  EnvBuilderContext {
    initCabal :: InitCabal,
    env :: EnvContext,
    initialState :: Initial EnvState
  }

data EnvBuilder =
  EnvBuilder {
    state :: EnvState,
    cabal :: CabalHandlers,
    buildTargets :: Bool -> Versions -> [SolvedId] -> M (BuildResult, Overrides)
  }

data Builder =
  Builder {
    withEnvBuilder :: ∀ a . EnvBuilderContext -> (EnvBuilder -> Initial EnvState -> M a) -> M a
  }

runBuilder :: Builder -> EnvBuilderContext -> (EnvBuilder -> Initial EnvState -> M a) -> M a
runBuilder Builder {withEnvBuilder} = withEnvBuilder

data BuildHandlers =
  BuildHandlers {
    project :: ProjectHandlers,
    cabal :: ProjectPackages -> InitCabal,
    withBuilder :: ∀ a . (Builder -> M a) -> M a,
    versions :: AvailableVersionsHandlers
  }

testBuilder ::
  (Bool -> Versions -> [SolvedId] -> M (BuildResult, Overrides)) ->
  (Builder -> M a) ->
  M a
testBuilder buildTargets use =
  use Builder {
    withEnvBuilder = \ EnvBuilderContext {initCabal, env = EnvContext {ghc}, initialState} useE -> do
      ghcDb <- noteClient "Test builder needs an explicit GHC DB" ghc
      (cabal, _) <- initCabal.run mempty ghcDb
      useE EnvBuilder {state = initial initialState, ..} initialState
  }

versionsBuilder :: SourceHashHandlers -> (Versions -> M (BuildResult, Overrides)) -> (Builder -> M a) -> M a
versionsBuilder hackage build =
  testBuilder \ _ versions overrideVersions -> do
    overrides <- packageOverrides hackage mempty overrideVersions
    (status, revisions) <- build versions
    pure (status, revisions <> overrides)

handlersNull :: BuildHandlers
handlersNull =
  BuildHandlers {
    project = Project.handlersNull,
    cabal = \ _ -> InitCabal \ _ _ -> pure (Solve.handlersNull, mempty),
    withBuilder = testBuilder \ _ _ _ -> pure (BuildFailure UnknownFailure, mempty),
    versions = AvailableVersions.handlersNull
  }

wrapCabal :: (CabalHandlers -> CabalHandlers) -> BuildHandlers -> BuildHandlers
wrapCabal f BuildHandlers {..} =
  BuildHandlers {cabal = \ p -> InitCabal (\ d o -> first f <$> (cabal p).run d o), ..}

logCabal ::
  MonadIO m =>
  BuildHandlers ->
  m (IORef [(EnvConstraints, Maybe SolverPlan)], BuildHandlers)
logCabal handlers = do
  ref <- liftIO (newIORef [])
  pure (ref, wrapCabal (Cabal.logCabal ref) handlers)
