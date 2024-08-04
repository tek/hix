module Hix.Data.AppContext where

import Hix.Data.LogLevel (LogLevel)

data AppContext =
  AppContext {
    description :: Text,
    level :: LogLevel
  }
  deriving stock (Eq, Show)
