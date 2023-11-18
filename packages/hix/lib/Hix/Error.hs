module Hix.Error (
  module Hix.Data.Error,
  module Hix.Error,
) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Exon (exon)
import Path (Path, toFilePath)
import System.IO.Error (tryIOError)

import Hix.Data.Error (Error (..))
import qualified Hix.Log as Log

pathText :: Path b t -> Text
pathText =
  toText . toFilePath

prefixedError ::
  MonadIO m =>
  Text ->
  Text ->
  m ()
prefixedError desc msg =
  Log.error [exon|#{desc}: #{msg}|]

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

sourceError :: Text -> Path b t -> Text
sourceError reason source =
  [exon|#{reason} the source file '#{pathText source}'|]

tryIOWith ::
  (Text -> Text) ->
  IO a ->
  ExceptT Error IO a
tryIOWith mkErr ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> throwE (Fatal (mkErr (show err)))

tryIO ::
  IO a ->
  ExceptT Error IO a
tryIO =
  tryIOWith id

note :: Text -> Maybe a -> ExceptT Error IO a
note err =
  maybe (throwE (GhciError err)) pure

printError ::
  MonadIO m =>
  Bool ->
  Error ->
  m ()
printError verbose = \case
  PreprocError err -> printPreprocError err
  EnvError err -> printEnvError err
  GhciError err -> printGhciError err
  NewError err -> printNewError err
  BootstrapError err -> printBootstrapError err
  NoMatch msg | verbose -> printPreprocError msg
  NoMatch _ -> unit
  Fatal err -> printFatalError err
  Client err -> Log.error err
