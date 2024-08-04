module Hix.Integration.Data.Options where

import Path (Abs, File, Path)

import Hix.Data.GlobalOptions (GlobalOptions)

data HackageServeOptions =
  HackageServeOptions {
    portFile :: Maybe (Path Abs File)
  }
  deriving stock (Show, Generic)

data Command =
  HackageServe HackageServeOptions
  deriving stock (Show)

data Options =
  Options {
    global :: GlobalOptions,
    cmd :: Command
  }
  deriving stock (Show)
