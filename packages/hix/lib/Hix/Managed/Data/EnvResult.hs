module Hix.Managed.Data.EnvResult where

import Hix.Data.EnvName (EnvName)
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.StageResult (StageSummary)

data EnvResult =
  EnvResult {
    env :: EnvName,
    -- | The final state, which is 'Nothing' for stages that don't interact with it.
    state :: Maybe EnvState,
    summaries :: NonEmpty StageSummary
  }
  deriving stock (Eq, Show, Generic)
