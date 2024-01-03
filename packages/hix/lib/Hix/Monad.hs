module Hix.Monad (
  module Hix.Monad,
  AppResources (..),
  M,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
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
import Hix.Data.Error (Error (BootstrapError, Client, EnvError, GhciError, NewError))
import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions (GlobalOptions), defaultGlobalOptions)
import Hix.Data.Monad (AppResources (..), LogLevel, M (M), liftE)
import Hix.Error (Error (Fatal), tryIO, tryIOWith)
import qualified Hix.Log as Log
import Hix.Log (logWith)

throwM :: Error -> M a
throwM =
  liftE . throwE

note :: Error -> Maybe a -> M a
note err =
  maybe (throwM err) pure

noteEnv :: Text -> Maybe a -> M a
noteEnv err =
  note (EnvError err)

noteGhci :: Text -> Maybe a -> M a
noteGhci err =
  note (GhciError err)

noteNew :: Text -> Maybe a -> M a
noteNew err =
  note (NewError err)

noteBootstrap :: Text -> Maybe a -> M a
noteBootstrap err =
  note (BootstrapError err)

noteClient :: Text -> Maybe a -> M a
noteClient err =
  note (Client err)

noteFatal :: Text -> Maybe a -> M a
noteFatal err =
  note (Fatal err)

eitherClient :: Text -> Either Text a -> M a
eitherClient msg =
  leftA \ err -> throwM (Client [exon|#{msg}: #{err}|])

eitherFatal :: Text -> Either Text a -> M a
eitherFatal msg =
  leftA \ err -> throwM (Fatal [exon|#{msg}: #{err}|])

eitherFatalShow ::
  Show b =>
  Text ->
  Either b a ->
  M a
eitherFatalShow msg =
  eitherFatal msg . first show

whenDebug :: M () -> M ()
whenDebug m =
  whenM (M (asks (.debug))) do
    m

logIORef :: IORef [Text] -> LogLevel -> Text -> IO ()
logIORef ref _ msg =
  modifyIORef' ref (msg :)

withLogIORef :: ((LogLevel -> Text -> IO ()) -> IO a) -> IO ([Text], a)
withLogIORef use = do
  logRef <- newIORef []
  result <- use (logIORef logRef)
  log <- readIORef logRef
  pure (log, result)

runMLoggerWith :: (LogLevel -> Text -> IO ()) -> GlobalOptions -> M a -> IO (Either Error a)
runMLoggerWith logger GlobalOptions {..} (M ma) =
  withSystemTempDir "hix-cli" \ tmp ->
    runExceptT (runReaderT ma AppResources {logger = logWith logger, ..})

runMLogWith :: GlobalOptions -> M a -> IO ([Text], Either Error a)
runMLogWith opts ma =
  withLogIORef \ logger -> runMLoggerWith logger opts ma

runMLog :: Path Abs Dir -> M a -> IO ([Text], Either Error a)
runMLog = runMLogWith . defaultGlobalOptions

runMWith :: GlobalOptions -> M a -> IO (Either Error a)
runMWith = runMLoggerWith (const Console.err)

runM :: Path Abs Dir -> M a -> IO (Either Error a)
runM = runMWith . defaultGlobalOptions

runMDebug :: Path Abs Dir -> M a -> IO (Either Error a)
runMDebug cwd =
  runMWith (defaultGlobalOptions cwd) {GlobalOptions.verbose = True, GlobalOptions.debug = True}

tryIOMWithM :: (Text -> M a) -> IO a -> M a
tryIOMWithM handleError ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> handleError (show err)

tryIOMWith :: (Text -> Error) -> IO a -> M a
tryIOMWith mkErr ma = M (lift (tryIOWith mkErr ma))

tryIOMAs :: Error -> IO a -> M a
tryIOMAs err ma = do
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left exc -> do
      whenDebug do
        Log.error [exon|Replaced exception: #{show exc}|]
      throwM err

tryIOM :: IO a -> M a
tryIOM ma = M (lift (tryIO ma))

catchIOM :: IO a -> (Text -> M a) -> M a
catchIOM ma handle =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> handle (show err)

withTempDir :: String -> (Path Abs Dir -> M a) -> M a
withTempDir name use = do
  AppResources {tmp} <- M ask
  Path.withTempDir tmp name use

withTempFile :: String -> Maybe [Text] -> (Path Abs File -> M a) -> M a
withTempFile name content use = do
  AppResources {tmp} <- M ask
  Path.withTempFile tmp name \ file handle -> do
    for_ content \ lns -> liftIO (Text.hPutStr handle (Text.unlines lns))
    liftIO (hClose handle)
    use file

stateM ::
  Monad m =>
  (s -> a -> m (s, b)) ->
  a ->
  StateT s m b
stateM f a = do
  s <- get
  (s', b) <- lift (f s a)
  put s'
  pure b

mapAccumM ::
  Traversable t =>
  Monad m =>
  (s -> a -> m (s, b)) ->
  s ->
  t a ->
  m (s, t b)
mapAccumM f s as =
  swap <$> runStateT (traverse (stateM f) as) s
