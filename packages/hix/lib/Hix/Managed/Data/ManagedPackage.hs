module Hix.Managed.Data.ManagedPackage where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.List.Extra (nubOrdOn)
import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import GHC.IsList (IsList)
import Text.PrettyPrint (brackets, (<+>))

import Hix.Class.Map (LookupMaybe, NMap, nFromList)
import qualified Hix.Data.Dep
import Hix.Data.Dep (Dep)
import Hix.Data.Json (jsonParsec)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (LocalPackage (LocalPackage))
import Hix.Data.Version (Version)
import Hix.Managed.Data.Packages (Packages (..))
import Hix.Managed.Data.Targets (Targets)
import Hix.Managed.Targets (onlyTargets)
import Hix.Pretty (HPretty, prettyL)

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

newtype ProjectPackages =
  ProjectPackages (Packages ManagedPackage)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, Pretty, HPretty)

instance NMap ProjectPackages LocalPackage ManagedPackage LookupMaybe where

newtype EnvPackages =
  EnvPackages (Packages ManagedPackage)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, Pretty, HPretty)

instance NMap EnvPackages LocalPackage ManagedPackage LookupMaybe where

managedPackages :: Map (LocalPackage, Version) [Dep] -> ProjectPackages
managedPackages spec =
  nFromList (pkg <$> Map.toList spec)
  where
    pkg ((name, version), deps) =
      (name, ManagedPackage {name, version, deps})

forEnv :: Targets -> ProjectPackages -> EnvPackages
forEnv targets =
  coerce @(Packages ManagedPackage) . onlyTargets targets . coerce @_ @(Packages ManagedPackage)
