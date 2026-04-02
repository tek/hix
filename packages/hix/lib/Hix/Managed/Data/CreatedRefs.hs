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
    tags :: Set Tag
  }
  deriving stock (Eq, Show, Generic)

instance Semigroup CreatedRefs where
  CreatedRefs b1 t1 <> CreatedRefs b2 t2 = CreatedRefs (b1 <> b2) (t1 <> t2)

instance Monoid CreatedRefs where
  mempty = CreatedRefs Set.empty Set.empty

noBranches :: CreatedRefs
noBranches = mempty

singleTag :: Tag -> CreatedRefs
singleTag tag = CreatedRefs Set.empty (Set.singleton tag)

singleBranch :: BranchName -> CreatedRefs
singleBranch branch = CreatedRefs (Set.singleton branch) Set.empty

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
