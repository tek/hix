module Hix.Managed.Path where

import Path (Abs, Dir, Path)
import Path.IO (getCurrentDir)

import Hix.Data.Monad (M)
import Hix.Monad (tryIOM)

-- TODO find better home
rootOrCwd ::
  Maybe (Path Abs Dir) ->
  M (Path Abs Dir)
rootOrCwd =
  maybe (tryIOM getCurrentDir) pure
