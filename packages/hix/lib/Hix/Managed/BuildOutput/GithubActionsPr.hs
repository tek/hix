module Hix.Managed.BuildOutput.GithubActionsPr where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import Path (parseAbsFile)
import System.Environment (getEnv)
import System.Posix (epochTime)

import Hix.Data.Error (Error (Client, Fatal))
import Hix.Data.Monad (M)
import Hix.Data.OutputTarget (OutputTarget)
import Hix.Managed.BuildOutput.CommitMsg (commit)
import qualified Hix.Managed.Data.BuildOutput
import Hix.Managed.Data.BuildOutput (BuildOutput)
import Hix.Managed.Data.ManagedConfig (ManagedOp (..))
import Hix.Monad (note, throwM, tryIOMAs)
import qualified Hix.OutputWriter
import Hix.OutputWriter (OutputWriter, fileWriter, outputWriterM)

envVarWriter :: M OutputWriter
envVarWriter = do
  var <- tryIOMAs envVarError (getEnv "GITHUB_OUTPUT")
  outFile <- note envVarError (parseAbsFile var)
  pure (fileWriter writeError outFile)
  where
    envVarError = Client "The variable $GITHUB_OUTPUT does not contain a file path"
    writeError err = throwM (Fatal [exon|Couldn't write to $GITHUB_OUTPUT: #{err.msg}|])

formatOutput :: Text -> [Text] -> [Text]
formatOutput key = \case
  [value] -> [[exon|#{key}=#{value}|]]
  value -> [exon|#{key}<<EOF|] : value ++ ["EOF"]

formatOutputs :: Map Text [Text] -> [Text]
formatOutputs =
  concatMap (uncurry formatOutput) . Map.toList

writeOutputs :: OutputWriter -> Map Text [Text] -> M ()
writeOutputs writer values =
  writer.textAppend (Text.unlines (formatOutputs values))

modifiedOutputs :: ManagedOp -> Text -> Text -> [Text] -> Map Text [Text]
modifiedOutputs op date msg body =
  [
    ("branch", [[exon|hix-managed/#{opslug}-#{date}|]]),
    ("commit-message", [msg, ""] ++ body),
    ("title", [msg]),
    ("body", body),
    ("committer", ["hix <noreply@github.com>"]),
    ("signoff", ["false"]),
    ("delete-branch", ["true"])
  ]
  where
    opslug = case op of
      OpBump -> "bump"
      _ -> "lower"

githubActionsPr :: BuildOutput -> OutputTarget -> M ()
githubActionsPr output target =
  for_ (commit output) \ (msg, body) -> do
    date <- liftIO epochTime
    writer <- outputWriterM envVarWriter target
    writeOutputs writer (modifiedOutputs output.operation (show date) msg body)
