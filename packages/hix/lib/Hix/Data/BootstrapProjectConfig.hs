module Hix.Data.BootstrapProjectConfig where

import Hix.Data.NewProjectConfig (HixUrl)

data BootstrapProjectConfig =
  BootstrapProjectConfig {
    hixUrl :: HixUrl,
    noInitGitAndFlake :: Bool,
    devCli :: Bool
  }
  deriving stock (Eq, Show, Generic)
