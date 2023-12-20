module Hix.Managed.Handlers.Build where

import Hix.Class.Map (convert)
import qualified Hix.Data.ManagedEnv
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Monad (M)
import qualified Hix.Data.Overrides
import Hix.Data.Version (Versions)
import Hix.Managed.Data.BuildDomain (BuildDomain)
import Hix.Managed.Data.BuildState (BuildStatus (Failure))
import qualified Hix.Managed.Handlers.Hackage as HackageHandlers
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.StateFile as StateFileHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.Solve.Config (GhcDb)

data EnvBuilder =
  EnvBuilder {
    buildWithState :: ManagedState -> M BuildStatus,
    ghcDb :: M (Maybe GhcDb)
  }

data Builder =
  Builder {
    withEnvBuilder :: ∀ a . BuildDomain -> ManagedState -> (EnvBuilder -> M a) -> M a
  }

data BuildHandlers =
  BuildHandlers {
    stateFile :: StateFileHandlers,
    hackage :: HackageHandlers,
    withBuilder :: ∀ a . (Builder -> M a) -> M a
  }

testBuilder :: (ManagedState -> M BuildStatus) -> (Builder -> M a) -> M a
testBuilder buildWithState use =
  use Builder {withEnvBuilder = \ _ _ useE -> useE EnvBuilder {buildWithState, ghcDb = pure Nothing}}

versionsBuilder :: (Versions -> M BuildStatus) -> (Builder -> M a) -> M a
versionsBuilder build =
  testBuilder \ state -> build (convert (.version) state.overrides)

handlersNull :: BuildHandlers
handlersNull =
  BuildHandlers {
    stateFile = StateFileHandlers.handlersNull,
    hackage = HackageHandlers.handlersNull,
    withBuilder = testBuilder (const (pure Failure))
  }
