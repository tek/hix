module Hix.Managed.Build.Env where

import Path (Abs, Dir, File, Path)

import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers, tempProjectBracket)
import Hix.Managed.Handlers.Mutation (MutationHandlers)
import qualified Hix.Managed.Handlers.StateFile
import Hix.Monad (M)

data BuildEnv a s =
  BuildEnv {
    build :: BuildHandlers,
    mutation :: MutationHandlers a s,
    root :: Path Abs Dir,
    stateFile :: Path Abs File
  }

withBuildEnv ::
  BuildHandlers ->
  MutationHandlers a s ->
  StateFileConfig ->
  (BuildEnv a s -> M x) ->
  M x
withBuildEnv build mutation conf use =
  tempProjectBracket build.withTempProject conf.projectRoot \ root -> do
    stateFile <- build.stateFile.initFile root conf.file
    use BuildEnv {..}
