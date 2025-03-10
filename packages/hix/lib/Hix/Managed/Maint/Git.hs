module Hix.Managed.Maint.Git where

import Control.Monad.Catch (finally)
import qualified Data.Text as Text
import Distribution.Parsec (simpleParsec)
import Exon (exon)
import System.Posix (epochTime)

import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.BuildOutput.CommitMsg (commitModified)
import Hix.Managed.Data.BuildOutput (ModifiedId)
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.RevisionConfig (RevisionConfig (..))
import Hix.Managed.Git (
  BranchName (..),
  GitApi,
  GitNative (..),
  GitResult (..),
  MaintBranch (..),
  Tag,
  gitApi,
  gitApiHermetic,
  gitError,
  )
import Hix.Monad (clientError, fatalError)
import Hix.Pretty (showP)

data GitMaint =
  GitMaint {
    bracket :: ∀ a . M a -> M a,
    readTags :: M [Tag],
    listTargetBranches :: LocalPackage -> M [MaintBranch],
    branchOffTag :: MaintBranch -> Tag -> M (),
    switchBranch :: MaintBranch -> M BranchName,
    commitBump :: MaintBranch -> LocalPackage -> NonEmpty ModifiedId -> M BranchName
  }

data GitRevision =
  GitRevision {
    bracket :: ∀ a . M a -> M a,
    listTargetBranches :: LocalPackage -> M [MaintBranch],
    switchBranch :: MaintBranch -> M BranchName
  }

formatReleaseBranch :: MaintBranch -> Text
formatReleaseBranch MaintBranch {..} =
  [exon|release/##{package}/#{showP version}|]

releaseBranchName :: MaintBranch -> BranchName
releaseBranchName = BranchName . formatReleaseBranch

-- TODO make switching back to main configurable
cleanup :: GitNative -> Bool -> Maybe Text -> Maybe Text -> M ()
cleanup git push initialBranch mainRemote = do
  git.cmd_ ["reset", "--hard"]
  for_ initialBranch \ branch -> git.cmd_ ["switch", "-f", branch]
  when push do
    for_ mainRemote \ remote -> git.cmd_ ["push", "--all", remote]

branchRemote :: GitNative -> Text -> M (Maybe Text)
branchRemote git branch =
  git.cmdResult args >>= \case
    GitSuccess remotes -> pure (head remotes)
    GitFailure {code = 1} -> pure Nothing
    GitFailure {..} -> fatalError (gitError args stdout stderr)
  where
    args = ["config", [exon|branch.#{branch}.remote|]]

listTargetBranches :: GitNative -> LocalPackage -> M [MaintBranch]
listTargetBranches git target = do
  allBranches <- git.cmd ["branch", "--list", "--format=%(refname:short)", [exon|release/##{target}/*|]]
  pure (mapMaybe (simpleParsec . toString) allBranches)

switchBranch :: GitNative -> MaintBranch -> M BranchName
switchBranch git branch = do
  let branchName = releaseBranchName branch
  git.cmd_ ["switch", showP branchName]
  pure branchName

gitMaintNative :: MaintConfig -> GitNative -> GitMaint
gitMaintNative MaintConfig {push, fetch, pr} git =
  GitMaint {
    listTargetBranches = listTargetBranches git,
    switchBranch = switchBranch git,
    ..
  }
  where
    bracket :: ∀ a . M a -> M a
    bracket ma = do
      git.cmd' ["diff", "--exit-code"] >>= leftA \ (out, err) ->
        clientError [exon|Git tree is dirty:
stdout: #{Text.unlines out}
stderr: #{Text.unlines err}|]
      when fetch do
        git.cmd_ ["fetch", "--tags", "origin", "release/*:release/*"]
      initialBranch <- head <$> git.cmd ["symbolic-ref", "--short", "HEAD"]
      mainRemote <- traverse (branchRemote git) initialBranch
      finally ma (cleanup git push initialBranch (join mainRemote))

    readTags = mapMaybe (simpleParsec . toString) <$> git.cmd ["tag", "--list", "--format=%(refname:short)"]

    branchOffTag branch tag = do
      let branchName = formatReleaseBranch branch
      git.cmd_ ["switch", "--create", branchName, showP tag]

    commitBump branch package modified = do
      prBranch <- ensurePrBranch branch
      let (message, body) = commitModified [[exon|Maintenance for '##{package}'|]] modified
      git.cmd_ ["add", "--all"]
      git.cmd_ ["commit", "-m", message, "-m", Text.unlines body]
      pure prBranch

    ensurePrBranch branch
      | pr = do
        date <- liftIO epochTime
        let prBranch = [exon|#{releaseBranch}-#{show date}|]
        git.cmd_ ["switch", "--create", coerce prBranch]
        pure prBranch
      | otherwise
      = pure releaseBranch
      where
        releaseBranch = releaseBranchName branch

gitApiMaintProd :: MaintConfig -> GitApi GitMaint
gitApiMaintProd config = gitApi (gitMaintNative config)

gitApiMaintHermetic :: MaintConfig -> GitApi GitMaint
gitApiMaintHermetic config = gitApiHermetic (gitMaintNative config)

gitRevisionNative :: RevisionConfig -> GitNative -> GitRevision
gitRevisionNative RevisionConfig {fetch} git =
  GitRevision {
    listTargetBranches = listTargetBranches git,
    switchBranch = switchBranch git,
    ..
  }
  where
    bracket :: ∀ a . M a -> M a
    bracket ma = do
      git.cmd' ["diff", "--exit-code"] >>= leftA \ (out, err) ->
        clientError [exon|Git tree is dirty:
stdout: #{Text.unlines out}
stderr: #{Text.unlines err}|]
      when fetch do
        git.cmd_ ["fetch", "--tags", "origin", "release/*:release/*"]
      initialBranch <- head <$> git.cmd ["symbolic-ref", "--short", "HEAD"]
      mainRemote <- traverse (branchRemote git) initialBranch
      finally ma (cleanup git False initialBranch (join mainRemote))

gitApiRevisionProd :: RevisionConfig -> GitApi GitRevision
gitApiRevisionProd config = gitApi (gitRevisionNative config)

gitApiRevisionHermetic :: RevisionConfig -> GitApi GitRevision
gitApiRevisionHermetic config = gitApiHermetic (gitRevisionNative config)
