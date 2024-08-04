module Hix.Managed.BuildOutput.CommitMsg where

import qualified Data.Text as Text
import Exon (exon)

import Hix.Data.PackageName (PackageName (..))
import qualified Hix.Managed.Data.BuildOutput
import Hix.Managed.Data.BuildOutput (DepChanges, ModifiedId)
import Hix.Managed.Data.Mutable (MutableDep (..), depName)
import Hix.Pretty (showP)

commitMessage :: Int -> Text -> Text
commitMessage num names
  | num <= 3
  = [exon|Bump #{names}|]
  | otherwise
  = [exon|Bump #{show num} dependencies|]

candidateList :: [ModifiedId] -> [Text]
candidateList = fmap \ c -> [exon|* #{showP c}|]

commitBody :: [Text] -> [ModifiedId] -> [Text]
commitBody preamble candidates =
  preamble ++ space ++ ["New versions:", ""] ++ candidateList candidates
  where
    space = if null preamble then [] else [""]

formatMutableDepsNE :: NonEmpty MutableDep -> Text
formatMutableDepsNE =
  Text.intercalate ", " . fmap (coerce . depName) . toList

formatMutableDeps :: [MutableDep] -> Maybe Text
formatMutableDeps =
  fmap formatMutableDepsNE . nonEmpty

modifiedIdNamesNE :: NonEmpty ModifiedId -> Text
modifiedIdNamesNE modified =
  formatMutableDepsNE ((.package) <$> modified)

modifiedIdNames :: [ModifiedId] -> Maybe Text
modifiedIdNames modified =
  formatMutableDeps ((.package) <$> modified)

commitModified :: [Text] -> NonEmpty ModifiedId -> (Text, [Text])
commitModified preamble modified =
  (commitMessage (length modified) (modifiedIdNamesNE modified), commitBody preamble (toList modified))

commit :: DepChanges -> Maybe (Text, [Text])
commit changes =
  commitModified [] <$> nonEmpty changes.modified

formatCommit :: DepChanges -> Maybe Text
formatCommit output =
  commit output <&> \ (msg, body) ->
    Text.intercalate "\n" ([msg, ""] ++ body)
