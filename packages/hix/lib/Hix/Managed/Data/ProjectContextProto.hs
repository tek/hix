module Hix.Managed.Data.ProjectContextProto where

import Data.Aeson (FromJSON)

import Hix.Class.Map (nGet)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ManagedPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Pretty (HPretty (hpretty), field, prettyMap)

data ProjectContextProto =
  ProjectContextProto {
    packages :: Packages ManagedPackage,
    state :: ProjectStateProto,
    envs :: Envs EnvConfig,
    hackage :: Map HackageName ContextHackageRepo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

instance Default ProjectContextProto where
  def =
    ProjectContextProto {
      packages = mempty,
      state = def,
      envs = mempty,
      hackage = mempty
    }

instance HPretty ProjectContextProto where
  hpretty ProjectContextProto {..} =
    prettyMap "project context" [
      field "packages" packages,
      field "state" state,
      field "envs" (nGet envs),
      field "hackage" hackage
    ]
