module Hix.Monad (
  module Hix.Monad,
  AppResources (..),
  M,
) where

import Control.Lens ((%~))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadTransControl, controlT)
import Control.Monad.Trans.Except (ExceptT (ExceptT), catchE, runExceptT, throwE)
import Control.Monad.Trans.Identity (IdentityT (..))
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State.Strict (StateT, get, put, runStateT)
import Data.Char (toUpper)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, Dir, File, Path, SomeBase (Abs))
import qualified Path.IO as Path
import Path.IO (resolveDir', withSystemTempDir)
import System.IO (hClose)
import System.IO.Error (tryIOError)

import qualified Hix.Console as Console
import Hix.Data.AppContext (AppContext (..))
import Hix.Data.Error (Error (..), ErrorContext (..), ErrorMessage (..))
import qualified Hix.Data.GlobalOptions as GlobalOptions
import Hix.Data.GlobalOptions (GlobalOptions (GlobalOptions), defaultGlobalOptions)
import Hix.Data.LogLevel (LogLevel (..))
import Hix.Data.Monad (AppResources (..), M (M), appRes, liftE)
import Hix.Data.PathSpec (PathSpec (PathConcrete), resolvePathSpec')
import qualified Hix.Error as Error
import Hix.Error (tryIOWith)
import qualified Hix.Log as Log
import Hix.Log (logWith)
import Hix.Maybe (fromMaybeA)

errorContext :: M ErrorContext
errorContext = ErrorContext <$> appRes.context

errorLevel ::
  LogLevel ->
  M a ->
  M a
errorLevel new (M ma) = do
  res <- ask
  M (lift (Error.errorLevel new (runReaderT ma res)))

throwMError :: Error -> M a
throwMError = liftE . throwE

throwM :: ErrorMessage -> M a
throwM message = do
  context <- errorContext
  throwMError Error {level = Nothing, ..}

fromEitherError :: Either Error a -> M a
fromEitherError = leftA throwMError

fromEither :: Either ErrorMessage a -> M a
fromEither = leftA throwM

clientError :: Text -> M a
clientError msg = throwM (Client msg)

fatalError :: Text -> M a
fatalError msg = throwM (Fatal msg)

note :: ErrorMessage -> Maybe a -> M a
note err = fromMaybeA (throwM err)

noteClient :: Text -> Maybe a -> M a
noteClient = note . Client

noteFatal :: Text -> Maybe a -> M a
noteFatal = note . Fatal

eitherClient :: Either Text a -> M a
eitherClient = leftA clientError

eitherFatal :: Either Text a -> M a
eitherFatal = leftA fatalError

eitherFatalShow ::
  Show b =>
  Text ->
  Either b a ->
  M a
eitherFatalShow msg =
  eitherFatal . first mkMsg
  where
    mkMsg err = [exon|#{msg}: #{show err}|]

noteEnv :: Text -> Maybe a -> M a
noteEnv = noteFatal

noteGhci :: Text -> Maybe a -> M a
noteGhci = noteFatal

noteNew :: Text -> Maybe a -> M a
noteNew = noteFatal

noteBootstrap :: Text -> Maybe a -> M a
noteBootstrap = noteFatal

shouldLog :: LogLevel -> M Bool
shouldLog level =
  appRes.logLevel <&> \ logLevel -> level >= logLevel

whenDebug :: M () -> M ()
whenDebug m =
  whenM (shouldLog LogDebug) do
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

runMUsing :: AppResources -> M a -> IO (Either Error a)
runMUsing res (M ma) =
  runExceptT (runReaderT ma res)

runMLoggerWith :: (LogLevel -> Text -> IO ()) -> GlobalOptions -> M a -> IO (Either Error a)
runMLoggerWith logger GlobalOptions {..} ma =
  withSystemTempDir "hix-cli" \ tmp -> runExceptT do
    resolvedCwd <- resolvePathSpec' resolveDir' cwd
    resolvedRoot <- resolvePathSpec' resolveDir' root
    let
      resources = AppResources {
        logger = logWith logger,
        context = [],
        cwd = resolvedCwd,
        root = resolvedRoot,
        ..
      }
    ExceptT (runMUsing resources ma)

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
  runMWith (defaultGlobalOptions cwd) {GlobalOptions.logLevel = LogDebug}

tryIOMWithM :: (Text -> M a) -> IO a -> M a
tryIOMWithM handleError ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> handleError (show err)

tryIOMWith :: (Text -> ErrorMessage) -> IO a -> M a
tryIOMWith consMessage ma = do
  context <- errorContext
  M (lift (tryIOWith (\ msg -> Error {context, level = Nothing, message = consMessage msg}) ma))

tryIOMAs :: ErrorMessage -> IO a -> M a
tryIOMAs err ma = do
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left exc -> do
      whenDebug do
        Log.error [exon|Replaced exception: #{show exc}|]
      throwM err

tryIOM :: IO a -> M a
tryIOM = tryIOMWith Fatal

catchIOM :: IO a -> (Text -> M a) -> M a
catchIOM ma handle =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> handle (show err)

withProjectRoot :: Path Abs Dir -> M a -> M a
withProjectRoot root = local \ res -> res {root}

withTempDir :: String -> (Path Abs Dir -> M a) -> M a
withTempDir name use = do
  AppResources {tmp} <- ask
  Path.withTempDir tmp name use

withTempFile :: String -> Maybe [Text] -> (Path Abs File -> M a) -> M a
withTempFile name content use = do
  AppResources {tmp} <- ask
  Path.withTempFile tmp name \ file handle -> do
    for_ content \ lns -> liftIO (Text.hPutStr handle (Text.unlines lns))
    liftIO (hClose handle)
    use file

withTempRoot :: String -> (Path Abs Dir -> M a) -> M a
withTempRoot name use =
  withTempDir name \ root -> withProjectRoot root (use root)

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

withLower :: (∀ b . (M a -> IO b) -> IO b) -> M a
withLower f = do
  res <- ask
  liftE (ExceptT (f (runMUsing res)))

withLowerE :: ((∀ b . M b -> ExceptT Error IO b) -> ExceptT Error IO a) -> M a
withLowerE f = do
  res <- ask
  liftE (f \ (M ma) -> runReaderT ma res)

withLowerTry' :: ((∀ b . M b -> IO (Either Error b)) -> IO a) -> M a
withLowerTry' f = do
  res <- ask
  tryIOM (f (runMUsing res))

withLowerTry :: (∀ b . (M a -> IO b) -> IO b) -> M a
withLowerTry f = do
  res <- ask
  fromEitherError =<< tryIOM (f (runMUsing res))

globalOptions :: M GlobalOptions
globalOptions =
  M $ asks \ AppResources {..} -> GlobalOptions {
    cwd = PathConcrete (Abs cwd),
    root = PathConcrete (Abs root),
    ..
    }

local :: (AppResources -> AppResources) -> M a -> M a
local f (M ma) =
  M (Reader.local f ma)

ask :: M AppResources
ask = M Reader.ask

catchM :: M a -> (Error -> M a) -> M a
catchM ma handle =
  withLowerE \ lower -> catchE (lower ma) (lower . handle)

catchingM :: (Error -> M a) -> M a -> M a
catchingM = flip catchM

appContextAtT ::
  MonadTransControl t =>
  LogLevel ->
  LogLevel ->
  Text ->
  t M a ->
  t M a
appContextAtT logLevel level description ma = do
  controlT \ lower -> do
    Log.logDecorated logLevel logMsg
    local (#context %~ (AppContext {description, level} :)) (lower ma)
  where
    logMsg = case Text.uncons description of
      Just (h, t) -> Text.cons (toUpper h) t
      Nothing -> description

appContextT ::
  MonadTransControl t =>
  Text ->
  t M a ->
  t M a
appContextT =
  appContextAtT LogVerbose LogError

appContextAt :: LogLevel -> LogLevel -> Text -> M a -> M a
appContextAt logLevel level description ma =
  runIdentityT do
    appContextAtT logLevel level description (IdentityT ma)

appContext :: Text -> M a -> M a
appContext = appContextAt LogVerbose LogError

appContextVerbose :: Text -> M a -> M a
appContextVerbose desc ma = do
  appContextAt LogDebug LogVerbose desc ma

appContextDebug :: Text -> M a -> M a
appContextDebug desc ma = do
  appContextAt LogTrace LogDebug desc ma
