module Hix.Managed.Data.ProjectContext where

import Hix.Data.EnvName (EnvName)
import Hix.Managed.Cabal.Data.Config (CabalConfig)
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.EnvContext (EnvContext)
import Hix.Managed.Data.ManagedPackage (ProjectPackages)
import Hix.Managed.Data.ProjectState (ProjectState)

data ProjectContext =
  ProjectContext {
    build :: BuildConfig,
    state :: ProjectState,
    packages :: ProjectPackages,
    envs :: NonEmpty (Either EnvName EnvContext),
    cabal :: CabalConfig
  }
  deriving stock (Eq, Show)
