module Hix.Managed.Handlers.Lower where

import Data.IORef (IORef)

import Hix.Data.Monad (M)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Handlers.Build as Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Cabal (CabalHandlers)

data LowerHandlers =
  LowerHandlers {
    build :: BuildHandlers,
    versions :: PackageName -> M [Version]
  }

data SpecialLowerHandlers =
  TestLowerHandlers
  deriving stock (Eq, Show)

handlersNull :: LowerHandlers
handlersNull =
  LowerHandlers {
    build = Build.handlersNull,
    versions = \ _ -> pure []
  }

wrapCabal :: (CabalHandlers -> CabalHandlers) -> LowerHandlers -> LowerHandlers
wrapCabal f LowerHandlers {..} =
  LowerHandlers {build = Build.wrapCabal f build, ..}

logCabal ::
  MonadIO m =>
  LowerHandlers ->
  m (IORef [(EnvConstraints, Maybe SolverPlan)], LowerHandlers)
logCabal handlers = do
  (ref, build) <- Build.logCabal handlers.build
  pure (ref, handlers {build})
