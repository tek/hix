module Hix.Monad (
  module Hix.Monad,
  Env (..),
  M,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, Dir, File, Path)
import qualified Path.IO as Path
import Path.IO (withSystemTempDir)
import System.IO (hClose)
import System.IO.Error (tryIOError)

import qualified Hix.Console as Console
import qualified Hix.Data.Error as Error
import Hix.Data.Error (Error (BootstrapError, Client, EnvError, GhciError, NewError))
import Hix.Data.Monad (Env (..), LogLevel, M)
import Hix.Error (tryIO, tryIOWith)
import qualified Hix.Log as Log
import Hix.Log (logWith)

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

noteClient :: Text -> Maybe a -> M a
noteClient err =
  maybe (throwM (Client err)) pure

noteFatal :: Text -> Maybe a -> M a
noteFatal err =
  maybe (throwM (Error.Fatal err)) pure

eitherFatal :: Text -> Either Text a -> M a
eitherFatal msg =
  leftA (\err -> throwM (Error.Fatal [exon|#{msg}: #{err}|]))

eitherFatalShow ::
  Show b =>
  Text ->
  Either b a ->
  M a
eitherFatalShow msg =
  eitherFatal msg . first show

whenDebug :: M () -> M ()
whenDebug m =
  whenM (asks (.debug)) do
    m

logIORef :: IORef [Text] -> LogLevel -> Text -> IO ()
logIORef ref _ msg =
  modifyIORef' ref (msg :)

runMLog :: Bool -> Bool -> Bool -> Path Abs Dir -> M a -> IO ([Text], Either Error a)
runMLog verbose debug quiet cwd ma = do
  logRef <- newIORef []
  withSystemTempDir "hix-cli" \ tmp -> do
    result <- runExceptT (runReaderT ma Env {logger = logWith (logIORef logRef), ..})
    log <- readIORef logRef
    pure (log, result)

runMWith :: Bool -> Bool -> Bool -> Path Abs Dir -> M a -> IO (Either Error a)
runMWith verbose debug quiet cwd ma =
  withSystemTempDir "hix-cli" \ tmp ->
    runExceptT (runReaderT ma Env {logger = logWith (const Console.err), ..})

runM :: Path Abs Dir -> M a -> IO (Either Error a)
runM = runMWith False False False

runMDebug :: Path Abs Dir -> M a -> IO (Either Error a)
runMDebug = runMWith True True False

tryIOMWith :: (Text -> Text) -> IO a -> M a
tryIOMWith mkErr ma = lift (tryIOWith mkErr ma)

tryIOMAs :: Text -> IO a -> M a
tryIOMAs err ma = do
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left exc -> do
      whenDebug do
        Log.error [exon|Replaced exception: #{show exc}|]
      throwM (Error.Fatal err)

tryIOM :: IO a -> M a
tryIOM ma = lift (tryIO ma)

withTempDir :: String -> (Path Abs Dir -> M a) -> M a
withTempDir name use = do
  Env {tmp} <- ask
  Path.withTempDir tmp name use

withTempFile :: String -> Maybe [Text] -> (Path Abs File -> M a) -> M a
withTempFile name content use = do
  Env {tmp} <- ask
  Path.withTempFile tmp name \ file handle -> do
    for_ content \ lns -> liftIO (Text.hPutStr handle (Text.unlines lns))
    liftIO (hClose handle)
    use file
