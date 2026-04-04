module Hix.Monad.Run where

import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Path (Abs, Dir, Path)
import Path.IO (resolveDir', withSystemTempDir)

import Hix.Data.Error (Error (..))
import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions (GlobalOptions), defaultGlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (AppResources (..), M)
import Hix.Data.PathSpec (resolvePathSpec')
import Hix.Handlers.Tui (TuiHandlers)
import Hix.Handlers.Tui.InMemory (withTuiInMemory)
import qualified Hix.Handlers.Tui.Prod as Tui
import Hix.Monad (local, runMUsing)
import qualified Hix.Tui as Tui

withModifiedTui :: (TuiHandlers M -> TuiHandlers M) -> M a -> M a
withModifiedTui f =
  local \ res -> res {tui = f res.tui}

runWithTui :: TuiHandlers M -> M a -> M a
runWithTui tui =
  withModifiedTui (const (Tui.withLevels tui))

runMTuiWith :: TuiHandlers M -> GlobalOptions -> M a -> IO (Either Error a)
runMTuiWith tui GlobalOptions {..} ma =
  withSystemTempDir "hix-cli" \ tmp -> runExceptT do
    resolvedCwd <- resolvePathSpec' resolveDir' cwd
    resolvedRoot <- resolvePathSpec' resolveDir' root
    let
      resources = AppResources {
        tui = Tui.withLevels tui,
        context = [],
        cwd = resolvedCwd,
        root = resolvedRoot,
        persistentUi = True,
        ..
      }
    ExceptT (runMUsing resources ma)

runMLogWith :: GlobalOptions -> M a -> IO ([Text], Either Error a)
runMLogWith opts ma = do
  base <- Tui.handlersProd
  withTuiInMemory base \ tui -> runMTuiWith tui opts ma

runMLog :: Path Abs Dir -> M a -> IO ([Text], Either Error a)
runMLog = runMLogWith . defaultGlobalOptions

runMWith :: GlobalOptions -> M a -> IO (Either Error a)
runMWith options ma = do
  tui <- Tui.handlersProd
  runMTuiWith tui options ma

runM :: Path Abs Dir -> M a -> IO (Either Error a)
runM = runMWith . defaultGlobalOptions

runMDebug :: Path Abs Dir -> M a -> IO (Either Error a)
runMDebug cwd =
  runMWith (defaultGlobalOptions cwd) {GlobalOptions.logLevel = LogDebug}
