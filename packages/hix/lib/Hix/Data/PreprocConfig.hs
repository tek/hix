module Hix.Data.PreprocConfig where

import Data.Aeson (FromJSON)

import Hix.Data.ComponentConfig (PackagesConfig)

data PreprocConfig =
  PreprocConfig {
    packages :: PackagesConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)
