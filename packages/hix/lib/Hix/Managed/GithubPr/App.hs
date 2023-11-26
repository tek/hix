module Hix.Managed.GithubPr.App where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Exon (exon)
import Path (Abs, File, Path, parseAbsFile, toFilePath)
import System.Environment (getEnv)

import Hix.Data.Error (Error (Client, Fatal))
import Hix.Data.Monad (M)
import Hix.Managed.BuildOutput (requireBatchLog)
import Hix.Managed.CommitMsg.App (commit)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.ManagedConfig (ManagedOp (..))
import Hix.Monad (note, tryIOMAs, tryIOMWith)
import System.Posix (epochTime)

formatOutput :: Text -> [Text] -> [Text]
formatOutput key = \case
  [value] -> [[exon|#{key}=#{value}|]]
  value -> [exon|#{key}<<EOF|] : value ++ ["EOF"]

formatOutputs :: Map Text [Text] -> [Text]
formatOutputs =
  concatMap (uncurry formatOutput) . Map.toList

writeOutputs :: Map Text [Text] -> M ()
writeOutputs values = do
  var <- tryIOMAs envVarError (getEnv "GITHUB_OUTPUT")
  outFile <- note envVarError (parseAbsFile var)
  tryIOMWith writeError (Text.appendFile (toFilePath outFile) (Text.unlines (formatOutputs values)))
  where
    envVarError = Client "The variable $GITHUB_OUTPUT does not contain a file path"
    writeError err = Fatal [exon|Couldn't write to $GITHUB_OUTPUT: #{err}|]

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
      OpLower -> "lower"

managedGithubPrCli :: Path Abs File -> M ()
managedGithubPrCli file = do
  output <- requireBatchLog file
  for_ (commit output) \ (msg, body) -> do
    date <- liftIO epochTime
    writeOutputs (modifiedOutputs output.operation (show date) msg body)
