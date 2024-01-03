module Hix.Managed.Data.ProjectContext where

import Hix.Data.EnvName (EnvName)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectState (ProjectState)

data ProjectContext =
  ProjectContext {
    build :: BuildConfig,
    state :: ProjectState,
    packages :: Packages ManagedPackage,
    envs :: NonEmpty (Either EnvName EnvContext)
  }
  deriving stock (Eq, Show)
