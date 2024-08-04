module Hix.Managed.BuildOutput.GithubActionsPr where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import System.Posix (epochTime)

import Hix.Data.Monad (M)
import Hix.Managed.BuildOutput.CommitMsg (commit)
import Hix.Managed.Data.BuildOutput (DepChanges)
import qualified Hix.OutputWriter
import Hix.OutputWriter (OutputWriter)

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

modifiedOutputs :: Text -> Text -> [Text] -> Map Text [Text]
modifiedOutputs date msg body =
  [
    ("branch", [[exon|hix-managed/bump-#{date}|]]),
    ("commit-message", [msg, ""] ++ body),
    ("title", [msg]),
    ("body", body),
    ("committer", ["hix <noreply@github.com>"]),
    ("signoff", ["false"]),
    ("delete-branch", ["true"])
  ]

githubActionsPr :: DepChanges -> M (Maybe (Map Text [Text]))
githubActionsPr changes =
  for (commit changes) \ (msg, body) -> do
    date <- liftIO epochTime
    pure (modifiedOutputs (show date) msg body)
