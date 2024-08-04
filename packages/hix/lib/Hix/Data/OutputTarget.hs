module Hix.Data.OutputTarget where

import Path (Abs, File, Path)

data OutputTarget =
  OutputDefault
  |
  OutputStdout
  |
  OutputFile (Path Abs File)
  |
  OutputGithub
  deriving stock (Eq, Show, Generic)
