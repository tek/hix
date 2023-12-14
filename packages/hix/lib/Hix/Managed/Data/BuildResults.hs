module Hix.Managed.Data.BuildResults where

import qualified Data.Map.Strict as Map

import Hix.Data.Deps (TargetDeps)
import Hix.Data.EnvName (EnvName)
import Hix.Data.ManagedEnv (ManagedEnvState)
import Hix.Managed.Data.BuildResult (BuildResult, finalState)
import Hix.Managed.Data.ManagedConfig (ManagedOp)
import Hix.Managed.State (envStateWithMutations)

data BuildResults a =
  BuildResults {
    envs :: Map EnvName (BuildResult a),
    state :: ManagedEnvState
  }
  deriving stock (Eq, Show, Generic)

initBuildResults :: ManagedEnvState -> BuildResults a
initBuildResults state =
  BuildResults {envs = mempty, state}

-- TODO EnvName in BuildResult?
updateBuildResults ::
  ManagedOp ->
  EnvName ->
  TargetDeps ->
  BuildResults a ->
  BuildResult a ->
  BuildResults a
updateBuildResults op env targetDeps pre result =
  BuildResults {
    envs = Map.insert env result pre.envs,
    state = envStateWithMutations op env targetDeps (fold (finalState result)) pre.state
  }
