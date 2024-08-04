module Hix.Data.LogLevel where

data LogLevel =
  LogError
  |
  LogWarn
  |
  LogInfo
  |
  LogVerbose
  |
  LogDebug
  |
  LogTrace
  deriving stock (Eq, Show, Generic, Ord)
