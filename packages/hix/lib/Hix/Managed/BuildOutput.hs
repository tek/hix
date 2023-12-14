module Hix.Managed.BuildOutput where

import qualified Data.Aeson as Aeson
import Data.Aeson (decodeFileStrict', encodeFile)
import Exon (exon)
import Path (Abs, File, Path, toFilePath)

import Hix.Data.Monad (M)
import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Data.OutputTarget (OutputTarget (..))
import Hix.Error (pathText)
import Hix.Managed.BuildOutput.CommitMsg (formatCommit)
import Hix.Managed.BuildOutput.GithubActionsPr (githubActionsPr)
import Hix.Managed.Data.BuildOutput (BuildOutput, combineBuildOutputs)
import qualified Hix.Monad as Monad
import Hix.Monad (catchIOM, noteClient)
import qualified Hix.OutputWriter
import Hix.OutputWriter (outputWriterGlobal)

outputResult :: BuildOutput -> OutputTarget -> OutputFormat -> M ()
outputResult output target = \case
  OutputNone -> unit
  OutputJson -> writer.bytes (toStrict (Aeson.encode output))
  OutputCommitMsg -> traverse_ writer.text (formatCommit output)
  OutputGaPr -> githubActionsPr output target
  where
    writer = outputWriterGlobal target

loadBatchLog :: Path Abs File -> M (Maybe BuildOutput)
loadBatchLog file =
  catchIOM (decodeFileStrict' (toFilePath file)) \ _ -> pure Nothing

requireBatchLog :: Path Abs File -> M BuildOutput
requireBatchLog file =
  noteClient [exon|No build output log found at #{pathText file}|] =<< loadBatchLog file

updateBatchLog :: BuildOutput -> Path Abs File -> M ()
updateBatchLog output file = do
  old <- loadBatchLog file
  combined <- maybe (pure output) combine old
  liftIO (encodeFile (toFilePath file) combined)
  where
    combine old = Monad.eitherClient "When combining logs" (combineBuildOutputs old output)
