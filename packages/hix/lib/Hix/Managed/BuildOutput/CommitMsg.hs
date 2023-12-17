module Hix.Managed.BuildOutput.CommitMsg where

import qualified Data.Text as Text
import Exon (exon)

import qualified Hix.Managed.Data.BuildOutput
import Hix.Managed.Data.BuildOutput (BuildOutput)
import Hix.Managed.Data.Candidate (Candidate)
import Hix.Managed.Data.ManagedOp (ManagedOp (..))
import Hix.Pretty (showP)

commitMessage :: ManagedOp -> Int -> Text -> Text
commitMessage op num names
  | num <= 3
  = [exon|#{action} #{names}|]
  | otherwise
  = [exon|#{action} #{show num} dependencies|]
  where
    action = case op of
      OpBump -> "Bump"
      _ -> "Adjust lower bounds of"

candidateList :: [Candidate] -> [Text]
candidateList = fmap \ c -> [exon|* #{showP c}|]

commitBody :: [Candidate] -> [Text]
commitBody candidates =
  ["New versions:", ""] ++ candidateList candidates

commit :: BuildOutput -> Maybe (Text, [Text])
commit output =
  output.modifiedNames <&> \ names ->
    (commitMessage output.operation (length output.modified) names, commitBody output.modified)

formatCommit :: BuildOutput -> Maybe Text
formatCommit output =
  commit output <&> \ (msg, body) ->
    Text.intercalate "\n" ([msg, ""] ++ body)
