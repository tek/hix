module Hix.Data.Error where

import GHC.Exts (IsList)

import Hix.Data.AppContext (AppContext)
import Hix.Data.LogLevel (LogLevel)

data ErrorMessage =
  Fatal Text
  |
  FatalExternal Text
  |
  Client Text
  deriving stock (Eq, Show, Generic)

newtype ErrorContext =
  ErrorContext [AppContext]
  deriving stock (Eq, Show)
  deriving newtype (IsList)

data Error =
  Error {
    message :: ErrorMessage,
    context :: ErrorContext,
    level :: Maybe LogLevel
  }
  deriving stock (Eq, Show)
