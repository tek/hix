module Hix.Managed.Maint.RevisionPlan where

import Control.Monad.Extra (mapMaybeM)
import Distribution.Client.Compat.Prelude (simpleParsec)
import Exon (exon)
import Path (Dir, Path, Rel)

import Hix.Class.Map (nElems, nTransformMulti, (!?))
import qualified Hix.Color as Color
import Hix.Data.EnvName (EnvName)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage (..), PackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Data.MaintContext (MaintContext (..), MaintPackage (..))
import Hix.Managed.Data.ManagedPackage (ManagedPackage (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Git (BranchName (..), MaintBranch (..))
import Hix.Managed.Maint.Git (GitRevision (..), formatReleaseBranch)
import Hix.Managed.Maint.MaintPlan (envForTarget)
import Hix.Monad (appContext, noteClient)

data BadSpecs =
  BadSpecs {
    packages :: Maybe (NonEmpty PackageName),
    branches :: Maybe (NonEmpty BranchName)
  }
  deriving stock (Eq, Show)

data RevisionTarget =
  RevisionTarget {
    package :: LocalPackage,
    version :: Version,
    path :: Path Rel Dir,
    branch :: BranchName,
    env :: EnvName
  }
  deriving stock (Eq, Show)

newtype BranchPrio =
  BranchPrio MaintBranch
  deriving stock (Eq, Show)

instance Ord BranchPrio where
  compare = coerce (comparing @_ @MaintBranch (.version) <> comparing (.package))

newestBranch :: [MaintBranch] -> Maybe MaintBranch
newestBranch =
  head . sortOn (Down . BranchPrio)

revisionTarget ::
  ([MaintBranch] -> Maybe MaintBranch) ->
  GitRevision ->
  Packages EnvName ->
  MaintPackage ->
  M (Maybe RevisionTarget)
revisionTarget choose git targetEnvs MaintPackage {..} = do
  branches <- git.listTargetBranches package.name
  for (choose branches) \ branch@MaintBranch {version} -> do
    env <- envForTarget targetEnvs package.name
    pure RevisionTarget {branch = BranchName (formatReleaseBranch branch), package = package.name, ..}

allCandidates ::
  GitRevision ->
  Packages EnvName ->
  Packages MaintPackage ->
  M [RevisionTarget]
allCandidates git targetEnvs packages =
  mapMaybeM (revisionTarget newestBranch git targetEnvs) (nElems packages)

resolveTargets ::
  NonEmpty (Either PackageName BranchName) ->
  GitRevision ->
  Packages EnvName ->
  Packages MaintPackage ->
  M [RevisionTarget]
resolveTargets spec git targetEnvs packages =
  for (toList spec) \case
    Right (BranchName branch) ->
      appContext [exon|validating the target branch spec #{Color.path branch}|] do
        specBranch <- noteClient (badBranch branch) (simpleParsec (toString branch))
        maintPackage <- noteClient (noPackage specBranch.package) (packages !? specBranch.package)
        noteClient (nonexistentBranch branch) =<< revisionTarget (find (specBranch ==)) git targetEnvs maintPackage
    Left package ->
      appContext [exon|validating the target package spec #{Color.package package}|] do
        maintPackage <- noteClient (noPackage package) (packages !? LocalPackage package)
        noteClient (noBranch package) =<< revisionTarget newestBranch git targetEnvs maintPackage
  where
    badBranch branch =
      let fmt = Color.path @Text "release/<package>/<version>"
      in [exon|Not a valid release branch: #{Color.path branch} (must be #{fmt})|]

    noPackage package =
      [exon|The project does not define a package named #{Color.package package}.|]

    nonexistentBranch branch =
      [exon|The branch #{Color.path branch} does not exist. #{explanation}|]

    noBranch package =
      let fmt = Color.path @Text [exon|release/##{package :: PackageName}/<version>|]
      in [exon|No release branch for the package #{Color.package package} exists (#{fmt}). #{explanation}|]

    explanation =
      let cmd = Color.shellCommand @Text ".#maint"
      in [exon|This command is intended to be run only on branches that were created by #{cmd}.|]

-- TODO probably better to split this, keeping the pure part resolving specs to packages here, and adding some analogue
-- to MaintPrep.
-- This function should then return (Specified package branch) for specs and (Candidate package) for each package when
-- no specs were given.
revisionPlan ::
  GitRevision ->
  MaintContext ->
  Maybe (NonEmpty (Either PackageName BranchName)) ->
  M (Maybe (NonEmpty RevisionTarget))
revisionPlan git MaintContext {packages, envs} specified = do
  appContext "resolving target specifications" do
    nonEmpty <$> maybe allCandidates resolveTargets specified git targetEnvs packages
  where
    targetEnvs = flip nTransformMulti envs \ envName targets -> [(t, envName) | t <- targets]
