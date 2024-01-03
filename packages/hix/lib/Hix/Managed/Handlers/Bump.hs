module Hix.Managed.Handlers.Bump where

import Data.IORef (IORef)

import Hix.Data.Monad (M)
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Cabal.Changes (SolverPlan)
import Hix.Managed.Data.Constraints (EnvConstraints)
import qualified Hix.Managed.Handlers.Build as Build
import Hix.Managed.Handlers.Build (BuildHandlers)
import Hix.Managed.Handlers.Cabal (CabalHandlers)

data BumpHandlers =
  BumpHandlers {
    build :: BuildHandlers,
    latestVersion :: PackageName -> M (Maybe Version)
  }

data SpecialBumpHandlers =
  TestBumpHandlers
  deriving stock (Eq, Show)

handlersNull :: BumpHandlers
handlersNull =
  BumpHandlers {
    build = Build.handlersNull,
    latestVersion = \ _ -> pure Nothing
  }

wrapCabal :: (CabalHandlers -> CabalHandlers) -> BumpHandlers -> BumpHandlers
wrapCabal f BumpHandlers {..} =
  BumpHandlers {build = Build.wrapCabal f build, ..}

logCabal ::
  MonadIO m =>
  BumpHandlers ->
  m (IORef [(EnvConstraints, Maybe SolverPlan)], BumpHandlers)
logCabal handlers = do
  (ref, build) <- Build.logCabal handlers.build
  pure (ref, handlers {build})
