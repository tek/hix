module Hix.Monad where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Path (Abs, Dir, Path)

import Hix.Data.Error (Error (BootstrapError, EnvError, GhciError, NewError), tryIO)

data Env =
  Env {
    cwd :: Path Abs Dir
  }
  deriving stock (Eq, Show, Generic)

type M a = ReaderT Env (ExceptT Error IO) a

throwM :: Error -> M a
throwM =
  lift . throwE

noteEnv :: Text -> Maybe a -> M a
noteEnv err =
  maybe (throwM (EnvError err)) pure

noteGhci :: Text -> Maybe a -> M a
noteGhci err =
  maybe (throwM (GhciError err)) pure

noteNew :: Text -> Maybe a -> M a
noteNew err =
  maybe (throwM (NewError err)) pure

noteBootstrap :: Text -> Maybe a -> M a
noteBootstrap err =
  maybe (throwM (BootstrapError err)) pure

runM :: Path Abs Dir -> M a -> IO (Either Error a)
runM root ma =
  runExceptT (runReaderT ma (Env root))

tryIOM :: IO a -> M a
tryIOM ma =
  lift (tryIO ma)
