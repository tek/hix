module Hix.Managed.BuildOutput.CommitMsg where

import qualified Data.Text as Text
import Exon (exon)

import qualified Hix.Managed.Data.BuildOutput
import Hix.Managed.Data.BuildOutput (BuildOutput, ModifiedId)
import Hix.Pretty (showP)

commitMessage :: Int -> Text -> Text
commitMessage num names
  | num <= 3
  = [exon|Bump #{names}|]
  | otherwise
  = [exon|Bump #{show num} dependencies|]

candidateList :: [ModifiedId] -> [Text]
candidateList = fmap \ c -> [exon|* #{showP c}|]

commitBody :: [ModifiedId] -> [Text]
commitBody candidates =
  ["New versions:", ""] ++ candidateList candidates

commit :: BuildOutput -> Maybe (Text, [Text])
commit output =
  output.modifiedNames <&> \ names ->
    (commitMessage (length output.modified) names, commitBody output.modified)

formatCommit :: BuildOutput -> Maybe Text
formatCommit output =
  commit output <&> \ (msg, body) ->
    Text.intercalate "\n" ([msg, ""] ++ body)
