module Hix.OutputWriter where

import qualified Data.ByteString as ByteString
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, File, Path, toFilePath)

import qualified Hix.Console as Console
import Hix.Data.Monad (M)
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Error (Error (Fatal), pathText)
import Hix.Monad (throwM, tryIOMWithM)

data WriteError =
  WriteError {
    msg :: Text,
    file :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)

data OutputWriter =
  OutputWriter {
    bytes :: ByteString -> M (),
    text :: Text -> M (),
    textAppend :: Text -> M ()
  }

fileWriter :: (WriteError -> M ()) -> Path Abs File -> OutputWriter
fileWriter errorHandler file =
  OutputWriter {
    bytes = handleError . ByteString.writeFile fp,
    text = handleError . Text.writeFile fp,
    textAppend = handleError . Text.appendFile fp
  }
  where
    handleError = tryIOMWithM \ err -> errorHandler WriteError {msg = show err, file}
    fp = toFilePath file

writeError :: WriteError -> M ()
writeError err =
  throwM (Fatal [exon|Couldn't write to #{pathText err.file}: #{err.msg}|])

stdoutWriter :: OutputWriter
stdoutWriter =
  OutputWriter {
    bytes = Console.bytesOut,
    text = Console.out,
    textAppend = Console.out
  }

outputWriterM :: M OutputWriter -> OutputTarget -> M OutputWriter
outputWriterM defaultWriter = \case
  OutputFile file -> pure (fileWriter writeError file)
  OutputStdout -> pure stdoutWriter
  OutputDefault -> defaultWriter

outputWriter :: OutputWriter -> OutputTarget -> OutputWriter
outputWriter defaultWriter = \case
  OutputFile file -> fileWriter writeError file
  OutputStdout -> stdoutWriter
  OutputDefault -> defaultWriter

outputWriterGlobal :: OutputTarget -> OutputWriter
outputWriterGlobal = outputWriter stdoutWriter
