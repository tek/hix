module Hix.Managed.Maint.Git where

import qualified Data.Text as Text
import Distribution.Parsec (simpleParsec)
import Exon (exon)
import System.Posix (epochTime)

import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.BuildOutput.CommitMsg (commitModified)
import Hix.Managed.Data.BuildOutput (ModifiedId)
import Hix.Managed.Data.CreatedRefs (newRefsRef)
import Hix.Managed.Data.MaintConfig (MaintConfig (..))
import Hix.Managed.Data.RevisionConfig (RevisionConfig (..))
import Hix.Managed.Git (BranchName (..), GitApi, GitNative (..), MaintBranch (..), Tag, gitApiHermeticM, gitApiM)
import Hix.Managed.Git.Native (GitBracketConfig (..), createBranch, wrapBracketWithRefs)
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

listTargetBranches :: GitNative -> LocalPackage -> M [MaintBranch]
listTargetBranches git target = do
  allBranches <- git.cmd ["branch", "--list", "--format=%(refname:short)", [exon|release/##{target}/*|]]
  pure (mapMaybe (simpleParsec . toString) allBranches)

switchBranch :: GitNative -> MaintBranch -> M BranchName
switchBranch git branch = do
  let branchName = releaseBranchName branch
  git.cmd_ ["switch", showP branchName]
  pure branchName

gitMaintNative :: MaintConfig -> GitNative -> M GitMaint
gitMaintNative MaintConfig {push, fetch, pr} git = do
  refsRef <- newRefsRef
  let bracketConfig = GitBracketConfig {push, fetch, merge = False}
  pure GitMaint {
    bracket = wrapBracketWithRefs git bracketConfig refsRef,
    readTags = mapMaybe (simpleParsec . toString) <$> git.cmd ["tag", "--list", "--format=%(refname:short)"],
    listTargetBranches = listTargetBranches git,
    switchBranch = switchBranch git,
    branchOffTag = branchOffTagImpl refsRef,
    commitBump = commitBumpImpl refsRef
  }
  where
    branchOffTagImpl refsRef branch tag = do
      let branchName = BranchName (formatReleaseBranch branch)
      createBranch git refsRef branchName (Just (showP tag))

    commitBumpImpl refsRef branch package modified = do
      prBranch <- ensurePrBranch refsRef branch
      let (message, body) = commitModified [[exon|Maintenance for '##{package}'|]] modified
      git.cmd_ ["add", "--all"]
      git.cmd_ ["commit", "-m", message, "-m", Text.unlines body]
      pure prBranch

    ensurePrBranch refsRef branch
      | pr = do
        date <- liftIO epochTime
        let prBranch = BranchName [exon|#{formatReleaseBranch branch}-#{show date}|]
        createBranch git refsRef prBranch Nothing
        pure prBranch
      | otherwise
      = pure (releaseBranchName branch)

gitApiMaintProd :: MaintConfig -> GitApi GitMaint
gitApiMaintProd config = gitApiM (gitMaintNative config)

gitApiMaintHermetic :: MaintConfig -> GitApi GitMaint
gitApiMaintHermetic config = gitApiHermeticM (gitMaintNative config)

gitRevisionNative :: RevisionConfig -> GitNative -> M GitRevision
gitRevisionNative RevisionConfig {fetch} git = do
  refsRef <- newRefsRef
  let bracketConfig = GitBracketConfig {push = False, fetch, merge = False}
  pure GitRevision {
    bracket = wrapBracketWithRefs git bracketConfig refsRef,
    listTargetBranches = listTargetBranches git,
    switchBranch = switchBranch git
  }

gitApiRevisionProd :: RevisionConfig -> GitApi GitRevision
gitApiRevisionProd config = gitApiM (gitRevisionNative config)

gitApiRevisionHermetic :: RevisionConfig -> GitApi GitRevision
gitApiRevisionHermetic config = gitApiHermeticM (gitRevisionNative config)
