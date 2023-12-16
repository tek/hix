module Hix.Managed.Data.ManagedPackage where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (intersectVersionRanges)
import GHC.Exts (IsList)

import Hix.Class.Map (LookupMaybe, NtMap, ntFromList, ntPretty)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep, mainDep)
import Hix.Data.Json (jsonParsec)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (LocalPackage (LocalPackage))
import Hix.Data.Version (Version)

data ManagedPackage =
  ManagedPackage {
    name :: LocalPackage,
    version :: Version,
    deps :: [Dep]
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ManagedPackage where
  pretty ManagedPackage {..} = pretty PackageId {name = coerce name, version}

uniqueDeps :: [Dep] -> [Dep]
uniqueDeps =
  Map.elems .
  Map.fromListWith merge .
  fmap \ d -> (d.package, d)
  where
    merge d1 d2 = mainDep d1.package (intersectVersionRanges d1.version d2.version)

instance FromJSON ManagedPackage where
  parseJSON =
    withObject "ManagedPackage" \ o -> do
      name <- o .: "name"
      version <- jsonParsec <$> o .: "version"
      deps <- uniqueDeps <$> o .: "deps"
      pure ManagedPackage {..}

newtype ManagedPackages =
  ManagedPackages (Map LocalPackage ManagedPackage)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, FromJSON)

instance NtMap ManagedPackages LocalPackage ManagedPackage LookupMaybe where

instance Pretty ManagedPackages where
  pretty = ntPretty

managedPackages :: Map (LocalPackage, Version) [Dep] -> ManagedPackages
managedPackages =
  ntFromList . fmap pkg . Map.toList
  where
    pkg ((name, version), deps) = (name, ManagedPackage {..})
