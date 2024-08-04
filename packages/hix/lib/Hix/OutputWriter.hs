module Hix.OutputWriter where

import qualified Data.ByteString as ByteString
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, File, Path, toFilePath, parseAbsFile)

import qualified Hix.Console as Console
import Hix.Data.Monad (M)
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Error (pathText, ErrorMessage (..))
import Hix.Monad (fatalError, tryIOMWithM, tryIOMAs, note, throwM)
import System.Environment (getEnv)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Hix.Data.OutputFormat (OutputFormat (..))
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

data WriteError =
  WriteError {
    msg :: Text,
    file :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)

data OutputWriter =
  OutputWriter {
    bytes :: ByteString -> M (),
    bytesAppend :: ByteString -> M (),
    text :: Text -> M (),
    textAppend :: Text -> M ()
  }

fileWriter :: (WriteError -> M ()) -> Path Abs File -> OutputWriter
fileWriter errorHandler file =
  OutputWriter {
    bytes = handleError . ByteString.writeFile fp,
    bytesAppend = handleError . ByteString.appendFile fp,
    text = handleError . Text.writeFile fp,
    textAppend = handleError . Text.appendFile fp
  }
  where
    handleError = tryIOMWithM \ err -> errorHandler WriteError {msg = show err, file}
    fp = toFilePath file

defaultWriteError :: WriteError -> M ()
defaultWriteError err =
  fatalError [exon|Couldn't write to #{pathText err.file}: #{err.msg}|]

stdoutWriter :: OutputWriter
stdoutWriter =
  OutputWriter {
    bytes = Console.bytesOut,
    bytesAppend = Console.bytesOut,
    text = Console.out,
    textAppend = Console.out
  }

githubActionsWriter :: M OutputWriter
githubActionsWriter = do
  var <- tryIOMAs envVarError (getEnv "GITHUB_OUTPUT")
  outFile <- note envVarError (parseAbsFile var)
  pure (fileWriter writeError outFile)
  where
    envVarError = Client "The variable $GITHUB_OUTPUT does not contain a file path"
    writeError err = throwM (Fatal [exon|Couldn't write to $GITHUB_OUTPUT: #{err.msg}|])

outputWriter :: M OutputWriter -> OutputTarget -> M OutputWriter
outputWriter defaultWriter = \case
  OutputFile file -> pure (fileWriter defaultWriteError file)
  OutputStdout -> pure stdoutWriter
  OutputDefault -> defaultWriter
  OutputGithub -> githubActionsWriter

outputWriterGlobal :: OutputTarget -> M OutputWriter
outputWriterGlobal = outputWriter (pure stdoutWriter)

-- TODO The Maybes have been ported from the previous implementation.
-- I think they ware supposed to encode that no changes were to be committed, but this should probably be abstracted
-- over by this new concept.
-- Also since maint does have a useful commit message output, there needs to be an error variant (although that could
-- just be an exception in the callback).
type OutputSpecial :: Type -> Type
data OutputSpecial a where
  OutputSpecialCommitMsg :: OutputSpecial (Maybe (Text, [Text]))
  OutputSpecialGaPr :: OutputSpecial (Maybe (Map Text [Text]))
  OutputSpecialGithubOutputKey :: OutputSpecial Text

newtype OutputAdapter a =
  OutputAdapter { write :: OutputWriter -> a -> M () }

instance Semigroup (OutputAdapter a) where
  OutputAdapter l <> OutputAdapter r = OutputAdapter \ writer a -> l writer a *> r writer a

adapterSpecial ::
  (∀ b . a -> OutputSpecial b -> M b) ->
  OutputSpecial c ->
  OutputAdapter c ->
  OutputAdapter a
adapterSpecial f special (OutputAdapter use) =
  OutputAdapter \ writer a -> do
    c <- f a special
    use writer c

adapterCommit :: OutputAdapter (Maybe (Text, [Text]))
adapterCommit =
  OutputAdapter \ writer -> traverse_ \ (msg, body) -> writer.text (Text.intercalate "\n" ([msg, ""] ++ body))

formatGaPrOutput :: Text -> [Text] -> [Text]
formatGaPrOutput key = \case
  [value] -> [[exon|#{key}=#{value}|]]
  value -> [exon|#{key}<<EOF|] : value ++ ["EOF"]

adapterGaPr :: OutputAdapter (Maybe (Map Text [Text]))
adapterGaPr =
  OutputAdapter \ writer -> traverse_ \ values -> writer.textAppend (Text.unlines (format values))
  where
    format = concatMap (uncurry formatGaPrOutput) . Map.toList

adapterGaKey :: OutputAdapter Text
adapterGaKey =
  OutputAdapter \ writer key -> do
    writer.textAppend key
    writer.textAppend "="

adapterJson :: ToJSON a => Bool -> OutputAdapter a
adapterJson append =
  OutputAdapter \ writer a -> write writer (toStrict (Aeson.encode a))
  where
    write = if append then (.bytesAppend) else (.bytes)

defaultAdapter ::
  ToJSON a =>
  (∀ b . a -> OutputSpecial b -> M b) ->
  OutputFormat ->
  OutputAdapter a
defaultAdapter special = \case
  OutputNone -> OutputAdapter \ _ _ -> unit
  OutputJson -> adapterJson False
  OutputCommitMsg -> adapterSpecial special OutputSpecialCommitMsg adapterCommit
  OutputGaPr -> adapterSpecial special OutputSpecialGaPr adapterGaPr

outputAdapter ::
  ToJSON a =>
  (∀ b . a -> OutputSpecial b -> M b) ->
  OutputTarget ->
  OutputFormat ->
  OutputAdapter a
outputAdapter special target format
  | OutputGithub <- target
  , OutputJson <- format
  = adapterSpecial special OutputSpecialGithubOutputKey adapterGaKey <> adapterJson True
  | otherwise
  = defaultAdapter special format

defaultWriterFor :: OutputFormat -> M OutputWriter
defaultWriterFor = \case
  OutputGaPr -> githubActionsWriter
  _ -> pure stdoutWriter

writeOutput ::
  ToJSON a =>
  (∀ b . a -> OutputSpecial b -> M b) ->
  OutputTarget ->
  OutputFormat ->
  a ->
  M ()
writeOutput special target format value = do
  writer <- outputWriter (defaultWriterFor format) target
  adapter.write writer value
  where
    adapter = outputAdapter special target format
