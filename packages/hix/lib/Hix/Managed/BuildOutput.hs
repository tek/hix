module Hix.Managed.BuildOutput where

import Control.Exception (catch)
import qualified Data.Aeson as Aeson
import Data.Aeson (decodeFileStrict', encodeFile)
import Exon (exon)
import Path (Abs, File, Path, toFilePath)
import System.IO.Error (IOError)

import qualified Hix.Console as Console
import Hix.Data.Monad (M)
import Hix.Data.OutputFormat (OutputFormat (..))
import Hix.Error (pathText)
import Hix.Managed.Data.Build (BuildOutput, combineBuildOutputs)
import qualified Hix.Monad as Monad
import Hix.Monad (noteClient)

outputResult :: BuildOutput -> OutputFormat -> M ()
outputResult output = \case
  OutputNone -> unit
  OutputJson -> Console.bytesOut (toStrict (Aeson.encode output))

loadBatchLog :: Path Abs File -> M (Maybe BuildOutput)
loadBatchLog file =
  liftIO (catch (decodeFileStrict' (toFilePath file)) \ (_ :: IOError) -> pure Nothing)

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
