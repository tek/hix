module Hix.Data.ConfigDeps where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import qualified Data.Map.Strict as Map
import Distribution.Pretty (Pretty (pretty))
import GHC.Exts (IsList)

import Hix.Class.Map (LookupMaybe, NtMap, ntPretty)
import Hix.Data.ComponentConfig (ComponentName)
import Hix.Data.Dep (Dep)
import Hix.Data.PackageName (LocalPackage)
import Hix.Pretty (prettyL)

-- | This type reflects the structure of deps in the module options.
-- It is converted to a type without the component level right after parsing.
newtype ConfigComponentDeps =
  ConfigComponentDeps [Dep]
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsList)

instance Pretty ConfigComponentDeps where
  pretty (ConfigComponentDeps deps) = prettyL deps

instance FromJSON ConfigComponentDeps where
  parseJSON =
    withObject "ConfigComponentDeps" \ o ->
      ConfigComponentDeps <$> (parseJSON =<< o .: "dependencies")

newtype ConfigPackageDeps =
  ConfigPackageDeps (Map ComponentName ConfigComponentDeps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, IsList)

instance NtMap ConfigPackageDeps ComponentName ConfigComponentDeps LookupMaybe where

instance Pretty ConfigPackageDeps where
  pretty = ntPretty

type ConfigDeps = Map LocalPackage ConfigPackageDeps

configDeps :: Map LocalPackage (Map ComponentName [Dep]) -> ConfigDeps
configDeps =
  fmap (ConfigPackageDeps . fmap ConfigComponentDeps)

configLibDeps :: Map LocalPackage [Dep] -> ConfigDeps
configLibDeps =
  configDeps . fmap (Map.singleton "library")
