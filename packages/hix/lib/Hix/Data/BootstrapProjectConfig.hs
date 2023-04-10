module Hix.Data.BootstrapProjectConfig where

import Hix.Data.NewProjectConfig (HixUrl)

data BootstrapProjectConfig =
  BootstrapProjectConfig {
    hixUrl :: HixUrl
  }
  deriving stock (Eq, Show, Generic)
