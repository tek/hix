module Hix.Managed.CommitMsg.App where

import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, File, Path)

import qualified Hix.Console as Console
import Hix.Data.Monad (M)
import Hix.Managed.BuildOutput (requireBatchLog)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (BuildOutput)
import Hix.Managed.Data.Candidate (Candidate)
import Hix.Managed.Data.ManagedConfig (ManagedOp (..))
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
      OpLower -> "Adjust lower bounds of"

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
    Text.unlines ([msg, ""] ++ body)

managedCommitMsgCli :: Path Abs File -> M ()
managedCommitMsgCli file = do
  output <- requireBatchLog file
  traverse_ Console.out (formatCommit output)
