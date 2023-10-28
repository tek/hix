module Hix.Data.Error where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Exon (exon)
import Path (Path, toFilePath)
import System.IO.Error (tryIOError)

import qualified Hix.Console as Console

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
  BumpError Text
  |
  NoMatch Text
  |
  Fatal Text
  deriving stock (Eq, Show, Generic)

pathText :: Path b t -> Text
pathText =
  toText . toFilePath

printError ::
  MonadIO m =>
  Text ->
  Text ->
  m ()
printError desc msg =
  Console.error [exon|#{desc}: #{msg}|]

printPreprocError ::
  MonadIO m =>
  Text ->
  m ()
printPreprocError =
  printError "Preprocessor generator failed"

printEnvError ::
  MonadIO m =>
  Text ->
  m ()
printEnvError =
  printError "Invalid env config"

printGhciError ::
  MonadIO m =>
  Text ->
  m ()
printGhciError =
  printError "Invalid ghci config"

printNewError ::
  MonadIO m =>
  Text ->
  m ()
printNewError =
  printError "Can't create new project"

printBootstrapError ::
  MonadIO m =>
  Text ->
  m ()
printBootstrapError =
  printError "Can't bootstrap project"

printBumpError ::
  MonadIO m =>
  Text ->
  m ()
printBumpError =
  printError "Bumping deps failed"

printFatalError ::
  MonadIO m =>
  Text ->
  m ()
printFatalError =
  printError "Fatal error"

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
