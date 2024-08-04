module Hix.Managed.Data.ManagedPackage where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.List.Extra (nubOrdOn)
import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Class.Map (nFromList)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Json (jsonParsec)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (LocalPackage (LocalPackage))
import Hix.Data.Version (Version)
import Hix.Managed.Data.Packages (Packages)
import Hix.Pretty (prettyL)

data ManagedPackage =
  ManagedPackage {
    name :: LocalPackage,
    version :: Version,
    deps :: [Dep]
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ManagedPackage where
  pretty ManagedPackage {..} = pretty PackageId {name = coerce name, version} <+> brackets (prettyL deps)

instance FromJSON ManagedPackage where
  parseJSON =
    withObject "ManagedPackage" \ o -> do
      name <- o .: "name"
      version <- jsonParsec <$> o .: "version"
      deps <- nubOrdOn (.package) <$> o .: "deps"
      pure ManagedPackage {..}

managedPackages :: Map (LocalPackage, Version) [Dep] -> Packages ManagedPackage
managedPackages spec =
  nFromList (pkg <$> Map.toList spec)
  where
    pkg ((name, version), deps) =
      (name, ManagedPackage {name, version, deps})
