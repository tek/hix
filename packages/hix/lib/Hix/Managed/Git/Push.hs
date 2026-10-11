module Hix.Managed.Git.Push where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Exon (exon)

import Hix.Data.Monad (M)
import Hix.Managed.Git (GitNative (..), GitResult (..), gitError)
import qualified Hix.Log as Log
import Hix.Monad (fatalError)

-- | Extract the remote names of refs that were rejected from the output of @git push --porcelain@.
--
-- Each ref is reported as @<flag>\\t<from>:<to>\\t<summary>@, where the flag @!@ denotes a rejection.
-- All other lines (the remote URL, hints, @Done@) are ignored.
rejectedRefs :: [Text] -> [Text]
rejectedRefs output =
  [shortName (Text.drop 1 (snd (Text.breakOn ":" refspec))) | ["!", refspec, _] <- Text.splitOn "\t" <$> output]
  where
    shortName name = fromMaybe name (Text.stripPrefix "refs/heads/" name)

-- | Push refs to a remote, tolerating rejections of the refs in @tolerated@.
--
-- Refs are pushed non-atomically, so a rejected ref doesn't prevent the others from being updated.
-- Rejected refs that aren't tolerated, as well as failures that don't concern individual refs, are fatal.
pushRefs :: GitNative -> Set Text -> Text -> [Text] -> M ()
pushRefs git tolerated remote refs =
  git.cmdResult args >>= \case
    GitSuccess _ -> unit
    GitFailure {stdout, stderr}
      | rejected <- rejectedRefs stdout
      , not (null rejected)
      , all (flip Set.member tolerated) rejected
      -> for_ rejected \ ref ->
        Log.warn [exon|Ignoring rejected push of '#{ref}': The remote has diverged from the local state.|]
      | otherwise
      -> fatalError (gitError args stdout stderr)
  where
    args = ["push", "--porcelain", remote] ++ refs
