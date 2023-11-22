module Hix.Data.Monad where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Path (Abs, Dir, Path)

import Hix.Data.Error (Error)
import Hix.Data.OutputFormat (OutputFormat)

data LogLevel =
  LogError
  |
  LogWarn
  |
  LogInfo
  |
  LogVerbose
  |
  LogDebug
  deriving stock (Eq, Show, Generic)

data Env =
  Env {
    cwd :: Path Abs Dir,
    tmp :: Path Abs Dir,
    verbose :: Bool,
    debug :: Bool,
    quiet :: Bool,
    output :: OutputFormat,
    logger :: LogLevel -> Text -> M ()
  }

type M a = ReaderT Env (ExceptT Error IO) a
