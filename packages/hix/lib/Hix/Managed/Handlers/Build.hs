module Hix.Managed.Handlers.Build where

import Data.Aeson (FromJSON)
import Data.IORef (IORef, newIORef)

import Hix.Data.Monad (M)
import Hix.Data.Overrides (Overrides)
import Hix.Data.Version (Versions)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Cabal.Data.Config (GhcDb)
import Hix.Managed.Data.Constraints (EnvConstraints)
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.StageState (BuildStatus (Failure))
import qualified Hix.Managed.Handlers.Cabal as Solve
import qualified Hix.Managed.Handlers.Cabal as Cabal
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import qualified Hix.Managed.Handlers.Hackage as HackageHandlers
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Report as Report
import Hix.Managed.Handlers.Report (ReportHandlers)
import qualified Hix.Managed.Handlers.StateFile as StateFileHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)

newtype BuildOutputsPrefix =
  BuildOutputsPrefix Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON)

data EnvBuilder =
  EnvBuilder {
    cabal :: CabalHandlers,
    buildWithState :: Versions -> Overrides -> M BuildStatus
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
    hackage :: HackageHandlers,
    report :: ReportHandlers,
    cabal :: Packages ManagedPackage -> GhcDb -> M CabalHandlers,
    withBuilder :: ∀ a . (Builder -> M a) -> M a
  }

testBuilder :: (Versions -> Overrides -> M BuildStatus) -> (Builder -> M a) -> M a
testBuilder buildWithState use =
  use Builder {withEnvBuilder = \ cabal _ _ useE -> useE EnvBuilder {cabal, buildWithState}}

versionsBuilder :: (Versions -> M BuildStatus) -> (Builder -> M a) -> M a
versionsBuilder build =
  testBuilder \ versions _ -> build versions

handlersNull :: BuildHandlers
handlersNull =
  BuildHandlers {
    stateFile = StateFileHandlers.handlersNull,
    hackage = HackageHandlers.handlersNull,
    report = Report.handlersNull,
    cabal = \ _ _ -> pure Solve.handlersNull,
    withBuilder = testBuilder \ _ _ -> pure Failure
  }

wrapCabal :: (CabalHandlers -> CabalHandlers) -> BuildHandlers -> BuildHandlers
wrapCabal f BuildHandlers {..} =
  BuildHandlers {cabal = \ p d -> f <$> cabal p d, ..}

-- TODO use the additive pattern of IORef exfiltration for other handlers?
logCabal ::
  MonadIO m =>
  BuildHandlers ->
  m (IORef [(EnvConstraints, Maybe SolverPlan)], BuildHandlers)
logCabal handlers = do
  ref <- liftIO (newIORef [])
  pure (ref, wrapCabal (Cabal.logCabal ref) handlers)
