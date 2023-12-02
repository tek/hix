module Hix.Data.Monad where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT, asks)
import GHC.Records (HasField (getField))
import Path (Abs, Dir, Path)

import Hix.Data.Error (Error)
import Hix.Data.OutputFormat (OutputFormat)
import Hix.Data.OutputTarget (OutputTarget)

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
    target :: OutputTarget,
    logger :: LogLevel -> Text -> M ()
  }

type M a = ReaderT Env (ExceptT Error IO) a

data EnvM = EnvM

instance HasField name Env a => HasField name EnvM (M a) where
  getField EnvM = asks (getField @name)

envM :: EnvM
envM = EnvM
