module Hix.Managed.Maint.Prep where

import Distribution.Pretty (Pretty (pretty))
import Exon (exon)

import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.Version (Version)
import qualified Hix.Log as Log
import Hix.Managed.Git (MaintBranch (..), Tag (..))
import Hix.Managed.Maint.Data.MaintEnv (MaintEnv (..))
import Hix.Managed.Maint.Data.MaintPrep (MaintPrep (..))
import Hix.Managed.Maint.Data.MaintTarget (MaintTarget (..))
import Hix.Managed.Maint.Git (GitMaint (..))

-- | Return all tags that satisfy:
-- - Version not smaller than the target's.
-- - Either:
--   - Package segment absent (shared release of all packages).
--     Format: 1.4.3
--   - Package matches target.
--     Format: pname-1.4.3
matchTags :: LocalPackage -> Version -> [Tag] -> [Tag]
matchTags package version =
  filter \ (Tag tagPackage tagVersion) ->
    matchPackage tagPackage && tagVersion <= version
  where
    matchPackage = \case
      Just tp -> package == tp
      Nothing -> True

newtype TagPrio =
  TagPrio Tag
  deriving stock (Eq, Show)

instance Ord TagPrio where
  compare = coerce (comparing @_ @Tag (.version) <> comparing (.package))

-- | Pick the tag with the highest version for the given package.
-- Both package-specific and shared tags are eligible.
-- If the latest shared tag's version is higher than the latest specific tag's, pick the shared tag, and vice versa.
-- If the latest shared and specific tags have the same version, pick the specific tag.
selectTag :: LocalPackage -> Version -> [Tag] -> Maybe Tag
selectTag package version =
  head .
  sortOn (Down . TagPrio) .
  matchTags package version

useBranch ::
  MaintEnv ->
  MaintBranch ->
  M MaintPrep
useBranch env targetBranch = do
  workBranch <- env.git.switchBranch targetBranch
  pure PrepBranch {targetBranch, workBranch}

createBranch ::
  MaintEnv ->
  Tag ->
  M MaintPrep
createBranch env tag = do
  env.git.branchOffTag branch tag
  useBranch env branch
  where
    branch = MaintBranch {package = env.target.package, version = tag.version}

-- TODO this needs to fetch those branches as well – in CI they probably won't be present initially
--
-- TODO also it needs to check out an existing branch
selectBranch ::
  MaintEnv ->
  Tag ->
  M MaintPrep
selectBranch env tag = do
  traceInputs
  branches <- env.git.listTargetBranches env.target.package
  maybe (createBranch env tag) (useBranch env) (find matchBranch branches)
  where
    traceInputs = Log.debugP [exon|Using tag '#{pretty tag}' for '#{pretty env.target.package}'|]

    matchBranch MaintBranch {version = bv} = bv == tag.version

maintPrep ::
  MaintEnv ->
  [Tag] ->
  LocalPackage ->
  Version ->
  M MaintPrep
maintPrep env tags package version =
  maybe (pure PrepNoTags) (selectBranch env) (selectTag package version tags)
