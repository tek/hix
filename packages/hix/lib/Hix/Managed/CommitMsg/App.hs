module Hix.Managed.CommitMsg.App where

import qualified Data.Text as Text
import Exon (exon)
import Path (Abs, File, Path)

import qualified Hix.Console as Console
import Hix.Data.Monad (M)
import Hix.Error (pathText)
import Hix.Managed.BuildOutput (loadBatchLog)
import qualified Hix.Managed.Data.Build
import Hix.Managed.Data.Build (BuildOutput)
import Hix.Managed.Data.Candidate (Candidate)
import Hix.Monad (noteClient)
import Hix.Pretty (showP)

commitHeadline :: Int -> Text -> Text
commitHeadline num names
  | num <= 3
  = [exon|Bump #{names}|]
  | otherwise
  = [exon|Bump #{show num} dependencies|]

commitDetails :: [Candidate] -> [Text]
commitDetails = fmap showP

formatCommitMsg :: BuildOutput -> Maybe Text
formatCommitMsg output =
  output.modifiedNames <&> \ names ->
    Text.unlines ([commitHeadline (length output.modified) names, "", "New versions:"] ++ commitDetails output.modified)

managedCommitMsgCli :: Path Abs File -> M ()
managedCommitMsgCli file = do
  output <- noteClient [exon|No build output log found at #{pathText file}|] =<< loadBatchLog file
  traverse_ Console.out (formatCommitMsg output)
