module Hix.Managed.Build.Env where

import Path (Abs, Dir, File, Path)

import Hix.Data.Monad (M)
import qualified Hix.Managed.Data.ManagedConfig
import Hix.Managed.Data.ManagedConfig (StateFileConfig)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers, tempProjectBracket)
import qualified Hix.Managed.Handlers.StateFile

data BuildEnv =
  BuildEnv {
    build :: BuildHandlers,
    root :: Path Abs Dir,
    stateFile :: Path Abs File
  }

withBuildEnv ::
  BuildHandlers ->
  StateFileConfig ->
  (BuildEnv -> M x) ->
  M x
withBuildEnv build conf use =
  tempProjectBracket build.withTempProject conf.projectRoot \ root -> do
    stateFile <- build.stateFile.initFile root conf.file
    use BuildEnv {..}
