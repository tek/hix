module Hix.Data.GhcidTest where

import Path (Abs, Dir, Path)

import Hix.Data.GhciConfig (GhciArgs)

data GhcidTest =
  GhcidTest {
    args :: GhciArgs,
    searchPath :: [Path Abs Dir],
    script :: Text,
    test :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data GhciRun =
  GhciRun {
    test :: GhcidTest,
    shell :: Text,
    run :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data GhcidRun =
  GhcidRun {
    cmdline :: Text,
    ghci :: GhciRun
  }
  deriving stock (Eq, Show, Generic)
