module Hix.Integration.Data.Options where

import Path (File)

import Hix.Data.GlobalOptions (GlobalOptions)
import Hix.Data.PathSpec (PathSpec)

data HackageServeOptions =
  HackageServeOptions {
    portFile :: Maybe (PathSpec File)
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
