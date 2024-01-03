module Hix.Managed.Data.ProjectContextProto where

import Data.Aeson (FromJSON)

import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackageProto (ManagedPackageProto)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Managed.Handlers.Build (BuildOutputsPrefix)

data ProjectContextProto =
  ProjectContextProto {
    packages :: Packages ManagedPackageProto,
    state :: ProjectStateProto,
    envs :: Envs EnvConfig,
    buildOutputsPrefix :: Maybe BuildOutputsPrefix
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
