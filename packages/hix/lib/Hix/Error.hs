module Hix.Error (
  module Hix.Data.Error,
  module Hix.Error,
) where

import Control.Monad.Trans.Except (ExceptT, throwE, withExceptT)
import Exon (exon)
import Path (Path, toFilePath)
import System.IO.Error (tryIOError)

import qualified Hix.Console as Console
import Hix.Console (errorMessage, withErrorChevrons)
import Hix.Data.AppContext (AppContext (..))
import Hix.Data.Error (Error (..), ErrorContext (..), ErrorMessage (..))
import Hix.Data.LogLevel (LogLevel)

pathText :: Path b t -> Text
pathText =
  toText . toFilePath

prefixedError ::
  MonadIO m =>
  Text ->
  Text ->
  m ()
prefixedError desc msg =
  Console.err (errorMessage [exon|#{desc}: #{msg}|])

printPreprocError ::
  MonadIO m =>
  Text ->
  m ()
printPreprocError =
  prefixedError "Preprocessor generator failed"

printEnvError ::
  MonadIO m =>
  Text ->
  m ()
printEnvError =
  prefixedError "Invalid env config"

printGhciError ::
  MonadIO m =>
  Text ->
  m ()
printGhciError =
  prefixedError "Invalid ghci config"

printNewError ::
  MonadIO m =>
  Text ->
  m ()
printNewError =
  prefixedError "Can't create new project"

printBootstrapError ::
  MonadIO m =>
  Text ->
  m ()
printBootstrapError =
  prefixedError "Can't bootstrap project"

printFatalError ::
  MonadIO m =>
  Text ->
  m ()
printFatalError =
  prefixedError "Fatal error"

printFatalWhenError ::
  MonadIO m =>
  Text ->
  Text ->
  m ()
printFatalWhenError context =
  prefixedError [exon|Fatal error when #{context}|]

sourceError :: Text -> Path b t -> Text
sourceError reason source =
  [exon|#{reason} the source file '#{pathText source}'|]

throwMessage :: ErrorMessage -> ExceptT Error IO a
throwMessage message =
  throwE Error {message, level = Nothing, context = []}

catchIO ::
  (Text -> ExceptT e IO a) ->
  IO a ->
  ExceptT e IO a
catchIO handleError ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> handleError (show err)

tryIOWith ::
  (Text -> e) ->
  IO a ->
  ExceptT e IO a
tryIOWith mkErr ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> throwE (mkErr (show err))

tryIOContext ::
  ErrorContext ->
  IO a ->
  ExceptT Error IO a
tryIOContext context =
  tryIOWith \ message -> Error {message = Fatal message, level = Nothing, ..}

tryIO :: IO a -> ExceptT Error IO a
tryIO = tryIOContext []

errorLevel ::
  LogLevel ->
  ExceptT Error IO a ->
  ExceptT Error IO a
errorLevel new =
  withExceptT \ Error {..} -> Error {level = Just new, ..}

formatError :: ErrorMessage -> Text
formatError = \case
  Fatal msg -> [exon|Fatal: #{msg}|]
  FatalExternal msg -> [exon|Fatal: #{msg}|]
  Client msg -> msg

printErrorWith ::
  Monad m =>
  (Text -> m ()) ->
  LogLevel ->
  Error ->
  m ()
printErrorWith printer logLevel Error {..}
  | Just messageLevel <- level
  , messageLevel < logLevel
  = unit
  | ErrorContext ctxLines <- context
  = do
    for_ (reverse ctxLines) \ ctx ->
      when (logLevel >= ctx.level) do
        printer (withErrorChevrons [exon|While #{ctx.description}|])
    printer (errorMessage (formatError message))

printError ::
  MonadIO m =>
  LogLevel ->
  Error ->
  m ()
printError = printErrorWith Console.err
