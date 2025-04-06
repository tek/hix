module Hix.Data.GlobalOptions where

import Path (Abs, Dir, Path, SomeBase (Abs))

import Hix.Data.LogLevel (LogLevel (LogInfo))
import Hix.Data.OutputFormat (OutputFormat (OutputNone))
import Hix.Data.OutputTarget (OutputTarget (OutputDefault))
import Hix.Data.PathSpec (PathSpec (PathConcrete))

data GlobalOptions =
  GlobalOptions {
    logLevel :: LogLevel,
    cabalVerbose :: Bool,
    cwd :: PathSpec Dir,
    root :: PathSpec Dir,
    output :: OutputFormat,
    target :: OutputTarget
  }
  deriving stock (Eq, Show, Generic)

defaultGlobalOptions :: Path Abs Dir -> GlobalOptions
defaultGlobalOptions cwd =
  GlobalOptions {
    logLevel = LogInfo,
    cabalVerbose = False,
    cwd = cwdSpec,
    root = cwdSpec,
    output = OutputNone,
    target = OutputDefault
  }
  where
    cwdSpec :: PathSpec Dir
    cwdSpec = PathConcrete (Abs cwd)
