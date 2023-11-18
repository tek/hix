module Hix.Managed.Handlers.Build where

import Path (Abs, Dir, Path)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Fatal))
import Hix.Data.Package (LocalPackage)
import Hix.Data.Version (NewVersion)
import qualified Hix.Managed.Handlers.Hackage as HackageHandlers
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Solve as SolveHandlers
import Hix.Managed.Handlers.Solve (SolveHandlers)
import qualified Hix.Managed.Handlers.StateFile as StateFileHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Monad (M, throwM)

newtype TempProjectBracket =
  TempProjectBracket (∀ a . Maybe (Path Abs Dir) -> (Path Abs Dir -> M a) -> M a)

tempProjectBracket :: ∀ a . TempProjectBracket -> Maybe (Path Abs Dir) -> (Path Abs Dir -> M a) -> M a
tempProjectBracket (TempProjectBracket f) = f

data BuildHandlers =
  BuildHandlers {
    stateFile :: StateFileHandlers,
    solve :: SolveHandlers,
    hackage :: HackageHandlers,
    withTempProject :: TempProjectBracket,
    buildProject :: Path Abs Dir -> EnvName -> LocalPackage -> NewVersion -> M Bool
  }

handlersNull :: BuildHandlers
handlersNull =
  BuildHandlers {
    stateFile = StateFileHandlers.handlersNull,
    solve = SolveHandlers.handlersNull,
    hackage = HackageHandlers.handlersNull,
    withTempProject = TempProjectBracket \ _ _ -> throwM (Fatal "not implemented: withTempProject"),
    buildProject = \ _ _ _ _ -> pure False
  }
