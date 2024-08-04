module Hix.Data.GlobalOptions where

import Path (Abs, Dir, Path)

import Hix.Data.LogLevel (LogLevel (LogInfo))
import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.OutputTarget (OutputTarget (OutputDefault))

data GlobalOptions =
  GlobalOptions {
    logLevel :: LogLevel,
    cabalVerbose :: Bool,
    cwd :: Path Abs Dir,
    root :: Path Abs Dir,
    output :: OutputFormat,
    target :: OutputTarget
  }
  deriving stock (Eq, Show, Generic)

defaultGlobalOptions :: Path Abs Dir -> GlobalOptions
defaultGlobalOptions cwd =
  GlobalOptions {
    logLevel = LogInfo,
    cabalVerbose = False,
    cwd,
    root = cwd,
    output = OutputNone,
    target = OutputDefault
  }
