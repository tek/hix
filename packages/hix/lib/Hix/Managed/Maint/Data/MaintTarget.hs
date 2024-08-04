module Hix.Managed.Maint.Data.MaintTarget where

import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import Path (Dir, Path, Rel)

import Hix.Data.EnvName (EnvName)
import Hix.Data.PackageId (PackageId (..))
import Hix.Data.PackageName (LocalPackage (..), PackageName, localPackageName)
import Hix.Data.Version (Version)
import Hix.Managed.Git (MaintBranch (..))

-- TODO prune unused fields
data MaintTarget =
  MaintTarget {
    package :: LocalPackage,
    version :: Version,
    deps :: Set PackageName,
    -- TODO this should just be for manual override
    branch :: Maybe MaintBranch,
    path :: Path Rel Dir,
    env :: EnvName
  }
  deriving stock (Eq, Show)

instance Pretty MaintTarget where
  pretty MaintTarget {..} =
    [exon|#{pretty pid}#{foldMap prettyBranch branch}|]
    where
      prettyBranch name = [exon| (#{pretty name})|]
      pid = PackageId {name = localPackageName package ,..}
