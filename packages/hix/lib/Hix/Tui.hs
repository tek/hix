module Hix.Tui where

import Control.Lens ((%~))
import Control.Monad.Trans.Reader (ask)

import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (AppResources (..), M (M))
import Hix.Handlers.Tui (TuiHandlers (..))

logWith :: (LogLevel -> Text -> M ()) -> LogLevel -> Text -> M ()
logWith handler level msg = do
  env <- M ask
  let
    minDebug = env.logLevel >= LogDebug
    minVerbose = env.logLevel >= LogVerbose
    minInfo = env.logLevel >= LogInfo
  case level of
    LogTrace | env.logLevel >= LogTrace -> accept
    LogDebug | minDebug -> accept
    LogVerbose | minVerbose -> accept
    LogInfo | minInfo -> accept
    LogWarn | minInfo -> accept
    LogError -> accept
    _ -> unit
  where
    accept = handler level msg

withLevels ::
  TuiHandlers M ->
  TuiHandlers M
withLevels =
  #log %~ logWith
