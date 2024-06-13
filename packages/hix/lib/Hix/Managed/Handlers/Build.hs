module Hix.Managed.Handlers.Build where

import Data.Aeson (FromJSON)
import Data.IORef (IORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version, Versions)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Cabal.Data.Config (GhcDb)
import Hix.Managed.Data.Constraints (EnvConstraints)
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.StageState (BuildFailure (UnknownFailure), BuildResult (BuildFailure))
import qualified Hix.Managed.Handlers.Cabal as Solve
import qualified Hix.Managed.Handlers.Cabal as Cabal
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import qualified Hix.Managed.Handlers.StateFile as StateFileHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.Overrides (packageOverrides)

newtype BuildOutputsPrefix =
  BuildOutputsPrefix Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON)

newtype BuildTimeout =
  BuildTimeout Int
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, FromJSON)

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
    stateFile :: StateFileHandlers,
    report :: ReportHandlers,
    cabal :: Packages ManagedPackage -> GhcDb -> M CabalHandlers,
    withBuilder :: ∀ a . (Builder -> M a) -> M a,
    versions :: PackageName -> M [Version],
    latestVersion :: PackageName -> M (Maybe Version)
  }

testBuilder ::
  (Bool -> Versions -> [PackageId] -> M (BuildResult, (Overrides, Set PackageId))) ->
  (Builder -> M a) ->
  M a
testBuilder buildWithState use =
  use Builder {withEnvBuilder = \ cabal _ _ useE -> useE EnvBuilder {cabal, buildWithState}}

versionsBuilder :: HackageHandlers -> (Versions -> M BuildResult) -> (Builder -> M a) -> M a
versionsBuilder hackage build =
  testBuilder \ _ versions overrideVersions -> do
    overrides <- packageOverrides hackage overrideVersions
    status <- build versions
    pure (status, (overrides, mempty))

handlersNull :: BuildHandlers
handlersNull =
  BuildHandlers {
    stateFile = StateFileHandlers.handlersNull,
    report = Report.handlersNull,
    cabal = \ _ _ -> pure Solve.handlersNull,
    withBuilder = testBuilder \ _ _ _ -> pure (BuildFailure UnknownFailure, mempty),
    versions = \ _ -> pure [],
    latestVersion = \ _ -> pure Nothing
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

data SpecialBuildHandlers =
  TestBumpHandlers
  deriving stock (Eq, Show)
