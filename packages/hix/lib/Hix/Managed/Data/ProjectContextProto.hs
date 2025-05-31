module Hix.Managed.Data.ProjectContextProto where

import Data.Aeson (FromJSON)

import Hix.Class.Map (nGet)
import Hix.Managed.Cabal.Data.ContextHackageRepo (ContextHackageRepo)
import Hix.Managed.Cabal.Data.HackageRepo (HackageName)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.ManagedPackage (ProjectPackages)
import Hix.Managed.Data.ProjectStateProto (ProjectStateProto)
import Hix.Pretty (HPretty (hpretty), field, prettyMap)

data ProjectContextProto =
  ProjectContextProto {
    packages :: ProjectPackages,
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
    prettyMap "project context proto" [
      field "packages" packages,
      field "state" state,
      field "envs" (nGet envs),
      field "hackage" hackage
    ]
