module Hix.Data.GhciTest where

import Path (Abs, Dir, File, Path)

import Hix.Data.GhciConfig (GhciArgs)

data GhciTest =
  GhciTest {
    args :: GhciArgs,
    searchPath :: [Path Abs Dir],
    script :: Text,
    test :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data GhciRun =
  GhciRun {
    test :: GhciTest,
    shell :: Text,
    run :: Maybe Text,
    scriptFile :: Path Abs File
  }
  deriving stock (Eq, Show, Generic)

data GhcidRun =
  GhcidRun {
    cmdline :: Text,
    ghci :: GhciRun
  }
  deriving stock (Eq, Show, Generic)
