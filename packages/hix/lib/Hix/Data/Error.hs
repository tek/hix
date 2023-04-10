module Hix.Data.Error where

import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Path, toFilePath)
import System.IO (stderr)
import System.IO.Error (tryIOError)

data Error =
  PreprocError Text
  |
  EnvError Text
  |
  GhciError Text
  |
  NewError Text
  |
  BootstrapError Text
  |
  NoMatch Text
  |
  Fatal Text
  deriving stock (Eq, Show, Generic)

pathText :: Path b t -> Text
pathText =
  toText . toFilePath

printPreprocError ::
  MonadIO m =>
  Text ->
  m ()
printPreprocError msg =
  liftIO (Text.hPutStrLn stderr [exon|>>> Preprocessor generator failed: #{msg}|])

printEnvError ::
  MonadIO m =>
  Text ->
  m ()
printEnvError msg =
  liftIO (Text.hPutStrLn stderr [exon|>>> Invalid env config: #{msg}|])

printGhciError ::
  MonadIO m =>
  Text ->
  m ()
printGhciError msg =
  liftIO (Text.hPutStrLn stderr [exon|>>> Invalid ghci config: #{msg}|])

printNewError ::
  MonadIO m =>
  Text ->
  m ()
printNewError msg =
  liftIO (Text.hPutStrLn stderr [exon|>>> Can't create new project: #{msg}|])

printBootstrapError ::
  MonadIO m =>
  Text ->
  m ()
printBootstrapError msg =
  liftIO (Text.hPutStrLn stderr [exon|>>> Can't bootstrap project: #{msg}|])

printFatalError ::
  MonadIO m =>
  Text ->
  m ()
printFatalError msg =
  liftIO (Text.hPutStrLn stderr [exon|>>> Fatal error: #{msg}|])

sourceError :: Text -> Path b t -> Text
sourceError reason source =
  [exon|#{reason} the source file '#{pathText source}'|]

tryIO ::
  IO a ->
  ExceptT Error IO a
tryIO ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> throwE (Fatal (show err))

note :: Text -> Maybe a -> ExceptT Error IO a
note err =
  maybe (throwE (GhciError err)) pure
