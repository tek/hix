module Hix.Managed.ProjectResult where

import qualified Hix.Managed.Data.Mutation
import Hix.Managed.Data.Mutation (FailedMutation)
import qualified Hix.Managed.Data.ProjectResult
import Hix.Managed.Data.ProjectResult (ProjectResult (ProjectResult))
import qualified Hix.Managed.EnvResult as EnvResult
import Hix.Managed.EnvResult (DepResults, normalizeDepResults)

grouped :: ProjectResult -> DepResults
grouped ProjectResult {envs} =
  normalizeDepResults (foldMap EnvResult.grouped envs)

failures :: ProjectResult -> [FailedMutation]
failures ProjectResult {envs} =
  sortOn (.package) (EnvResult.failures =<< toList envs)
