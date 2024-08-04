module Hix.Data.MDep where

import Distribution.Package (mainLibSet)
import Distribution.Pretty (Pretty, pretty)
import Distribution.Types.Dependency (Dependency (Dependency))

import qualified Hix.Data.PackageName as PackageName
import Hix.Data.VersionBounds (majorRange, VersionBounds)
import Hix.Data.PackageName (PackageName)

data MDep =
  MDep {
    package :: PackageName,
    bounds :: VersionBounds
  }
  deriving stock (Eq, Show, Generic)

toCabal :: MDep -> Dependency
toCabal MDep {..} =
  Dependency (PackageName.toCabal package) (majorRange bounds) mainLibSet

instance Pretty MDep where
  pretty = pretty . toCabal
