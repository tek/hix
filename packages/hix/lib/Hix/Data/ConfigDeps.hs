module Hix.Data.ConfigDeps where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))

import Hix.Data.ComponentConfig (ComponentName)
import Hix.Data.Dep (Dep)
import Hix.Data.Package (LocalPackage)

-- | This type reflects the structure of deps in the module options.
-- It is converted to a type without the component level right after parsing.
newtype ConfigComponentDeps =
  ConfigComponentDeps [Dep]
  deriving stock (Eq, Show, Generic)

instance FromJSON ConfigComponentDeps where
  parseJSON =
    withObject "ConfigComponentDeps" \ o ->
      ConfigComponentDeps <$> (parseJSON =<< o .: "dependencies")

newtype ConfigPackageDeps =
  ConfigPackageDeps (Map ComponentName ConfigComponentDeps)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON)

type ConfigDeps = Map LocalPackage ConfigPackageDeps
