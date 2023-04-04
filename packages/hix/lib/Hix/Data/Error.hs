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
  NoMatch Text
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

sourceError :: Text -> Path b t -> Text
sourceError reason source =
  [exon|#{reason} the source file '#{pathText source}'|]

tryIO ::
  (Text -> Error) ->
  IO a ->
  ExceptT Error IO a
tryIO f ma =
  liftIO (tryIOError ma) >>= \case
    Right a -> pure a
    Left err -> throwE (f (show err))

tryPreproc :: IO a -> ExceptT Error IO a
tryPreproc =
  tryIO PreprocError

note :: Text -> Maybe a -> ExceptT Error IO a
note err =
  maybe (throwE (GhciError err)) pure
