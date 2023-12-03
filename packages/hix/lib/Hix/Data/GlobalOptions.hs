module Hix.Data.GlobalOptions where

import Path (Abs, Dir, Path)

import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.OutputTarget (OutputTarget (OutputDefault))

data GlobalOptions =
  GlobalOptions {
    verbose :: Bool,
    debug :: Bool,
    quiet :: Bool,
    cwd :: Path Abs Dir,
    output :: OutputFormat,
    target :: OutputTarget
  }
  deriving stock (Eq, Show, Generic)

defaultGlobalOptions :: Path Abs Dir -> GlobalOptions
defaultGlobalOptions cwd =
  GlobalOptions {
    verbose = False,
    debug = False,
    quiet = False,
    cwd,
    output = OutputNone,
    target = OutputDefault
  }
