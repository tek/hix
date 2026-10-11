module Hix.Managed.Data.CreatedRefs where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Extra (atomicModifyIORef'_)
import qualified Data.Set as Set
import Exon (exon)

import Hix.Data.Monad (M)
import Hix.Managed.Git (BranchName, Tag)
import Hix.Pretty (showP)

data CreatedRefs =
  CreatedRefs {
    branches :: Set BranchName,
    -- | Branches that only serve as the base for other branches, so a remote that has diverged on them isn't an error.
    bases :: Set BranchName,
    tags :: Set Tag
  }
  deriving stock (Eq, Show, Generic)

instance Semigroup CreatedRefs where
  CreatedRefs b1 s1 t1 <> CreatedRefs b2 s2 t2 = CreatedRefs (b1 <> b2) (s1 <> s2) (t1 <> t2)

instance Monoid CreatedRefs where
  mempty = CreatedRefs Set.empty Set.empty Set.empty

noBranches :: CreatedRefs
noBranches = mempty

singleTag :: Tag -> CreatedRefs
singleTag tag = mempty {tags = Set.singleton tag}

singleBranch :: BranchName -> CreatedRefs
singleBranch branch = mempty {branches = Set.singleton branch}

-- | A branch that is only the base for other branches, see 'bases'.
singleBase :: BranchName -> CreatedRefs
singleBase branch = mempty {branches = Set.singleton branch, bases = Set.singleton branch}

refArgs :: CreatedRefs -> [Text]
refArgs CreatedRefs {branches, tags} =
  branchArgs ++ tagArgs
  where
    branchArgs = [showP b | b <- Set.toList branches]
    tagArgs = [[exon|tag #{showP t}|] | t <- Set.toList tags]

newRefsRef :: M (IORef CreatedRefs)
newRefsRef = liftIO (newIORef mempty)

accumulateRefs :: IORef CreatedRefs -> CreatedRefs -> M ()
accumulateRefs ref newRefs =
  liftIO $ atomicModifyIORef'_ ref \ old -> old <> newRefs

collectRefs :: IORef CreatedRefs -> M CreatedRefs
collectRefs ref = liftIO do
  refs <- readIORef ref
  writeIORef ref mempty
  pure refs
