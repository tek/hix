module Hix.Monad where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Exon (exon)
import Path (Abs, Dir, Path)

import qualified Hix.Console as Console
import Hix.Data.Error (Error (BootstrapError, BumpError, EnvError, GhciError, NewError), tryIO)

data Env =
  Env {
    cwd :: Path Abs Dir,
    verbose :: Bool,
    debug :: Bool
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

noteBump :: Text -> Maybe a -> M a
noteBump err =
  maybe (throwM (BumpError err)) pure

runMWith :: Bool -> Bool -> Path Abs Dir -> M a -> IO (Either Error a)
runMWith verbose debug root ma =
  runExceptT (runReaderT ma (Env root verbose debug))

runM :: Path Abs Dir -> M a -> IO (Either Error a)
runM = runMWith False False

runMDebug :: Path Abs Dir -> M a -> IO (Either Error a)
runMDebug = runMWith True True

tryIOM :: IO a -> M a
tryIOM ma =
  lift (tryIO ma)

logDebug :: Text -> M ()
logDebug msg =
  whenM (asks (.debug)) do
    Console.error [exon|debug: #{msg}|]
