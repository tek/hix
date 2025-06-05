module Hix.Managed.Git.Native where

import Control.Monad.Catch (finally)
import Data.IORef (IORef)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Exon (exon)

import Hix.Data.Monad (M)
import Hix.Managed.Data.CreatedRefs (CreatedRefs (..), accumulateRefs, collectRefs, singleBranch, singleTag)
import Hix.Managed.Git (BranchName (..), GitNative (..), GitResult (..), Tag (..), gitError)
import Hix.Monad (appContext, clientError, fatalError)
import Hix.Pretty (showP)

-- | Configuration for 'gitBracket' behavior
data GitBracketConfig =
  GitBracketConfig {
    -- | Push created branches and tags to remote
    push :: Bool,
    -- | Fetch from remote before starting
    fetch :: Bool,
    -- | Merge final branch back into initial branch before pushing
    merge :: Bool
  }
  deriving stock (Eq, Show)

-- | Create a new branch and track it in refs
createBranch ::
  GitNative ->
  IORef CreatedRefs ->
  BranchName ->
  -- | Optional base (tag/branch to branch from)
  Maybe Text ->
  M ()
createBranch git refsRef (BranchName branch) base = do
  let args = ["switch", "--create", branch] ++ maybe [] pure base
  git.cmd_ args
  accumulateRefs refsRef (singleBranch (BranchName branch))

-- | Create a git tag and accumulate it in the refs tracker
createTagWith ::
  GitNative ->
  IORef CreatedRefs ->
  Tag ->
  -- | Trailers
  [Text] ->
  M ()
createTagWith git refsRef tag trailers = do
  git.cmd_ (["tag", "-m", [exon|Release #{showP tag}|]] ++ concat [["--trailer", t] | t <- trailers] ++ [showP tag])
  accumulateRefs refsRef (singleTag tag)

branchRemote :: GitNative -> Text -> M (Maybe Text)
branchRemote git branch =
  git.cmdResult args >>= \case
    GitSuccess remotes -> pure (head remotes)
    GitFailure {code = 1} -> pure Nothing
    GitFailure {..} -> fatalError (gitError args stdout stderr)
  where
    args = ["config", [exon|branch.#{branch}.remote|]]

reset :: GitNative -> Maybe Text -> M ()
reset git initialBranch = do
  git.cmd_ ["reset", "--hard"]
  for_ initialBranch \ branch -> git.cmd_ ["switch", "-f", branch]

pushOnSuccess :: GitNative -> GitBracketConfig -> Maybe Text -> Maybe Text -> CreatedRefs -> M ()
pushOnSuccess git config initialBranch mainRemote refs = do
  branch <-
    if config.merge && hasRefs
    then mergeBranch
    else pure Nothing
  when (config.push && hasRefs) do
    push branch
  where
    mergeBranch = do
      for_ initialBranch \ branch -> do
        appContext [exon|merging into initial branch #{branch}|] do
          git.cmd_ ["switch", branch]
          -- Previous HEAD (the release branch)
          git.cmd_ ["merge", "--ff-only", "HEAD@{1}"]
      pure (BranchName <$> initialBranch)

    CreatedRefs {branches, tags} = refs

    hasRefs = not (Set.null branches && Set.null tags)

    push branch = do
      for_ mainRemote \ remote ->
        appContext [exon|pushing to git remote #{remote}|] do
          pushBranches remote branch
          pushTags remote

    pushBranches remote mergedBranch =
      unless (Set.null branchesToPush) do
        git.cmd_ $ ["push", remote] ++ [showP b | b <- Set.toList branchesToPush]
      where
        -- Include initial branch in push if we merged into it
        branchesToPush = case mergedBranch of
          Just branch -> Set.insert branch branches
          Nothing -> branches

    pushTags remote =
      unless (Set.null tags) do
        git.cmd_ $ ["push", remote] ++ [[exon|refs/tags/#{showP t}|] | t <- Set.toList tags]

gitBracket ::
  GitNative ->
  GitBracketConfig ->
  M (a, CreatedRefs) ->
  M a
gitBracket git config ma = do
  git.cmd' ["diff", "--exit-code"] >>= leftA \ (out, err) ->
    clientError [exon|Git tree is dirty:
stdout: #{Text.unlines out}
stderr: #{Text.unlines err}|]
  when config.fetch do
    git.cmd_ ["fetch", "--tags", "origin", "release/*:release/*"]
  initialBranch <- head <$> git.cmd ["symbolic-ref", "--short", "HEAD"]
  mainRemote <- traverse (branchRemote git) initialBranch
  finally (runAndPush initialBranch mainRemote) (reset git initialBranch)
  where
    runAndPush initialBranch mainRemote = do
      (result, refs) <- ma
      pushOnSuccess git config initialBranch (join mainRemote) refs
      pure result

-- | Wrap an action to collect created refs from an 'IORef' and pass them to 'gitBracket'
--
-- The 'IORef' is cleared before running the action and refs are collected after.
-- This allows git operations during the action to accumulate refs into the 'IORef'.
wrapBracketWithRefs ::
  GitNative ->
  GitBracketConfig ->
  IORef CreatedRefs ->
  (∀ a . M a -> M a)
wrapBracketWithRefs git config refsRef action = gitBracket git config do
  result <- action
  refs <- collectRefs refsRef
  pure (result, refs)
