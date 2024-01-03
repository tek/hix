module Hix.Managed.Data.ProjectResult where

import Hix.Managed.Data.EnvResult (EnvResult)
import Hix.Managed.Data.ProjectState (ProjectState)

data ProjectResult =
  ProjectResult {
    envs :: NonEmpty EnvResult,
    state :: ProjectState
  }
  deriving stock (Eq, Show, Generic)
