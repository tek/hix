module Hix.Data.Error where

import Distribution.Pretty (Pretty (..))
import GHC.Exts (IsList)
import Text.PrettyPrint (text, vcat, ($$), (<+>))

import Hix.Data.AppContext (AppContext (..))
import Hix.Data.LogLevel (LogLevel)

data ErrorMessage =
  Fatal Text
  |
  FatalExternal Text
  |
  Client Text
  deriving stock (Eq, Show, Generic)

instance Pretty ErrorMessage where
  pretty = \case
    Fatal msg -> fatal msg
    FatalExternal msg -> fatal msg
    Client msg -> text (toString msg)
    where
      fatal msg = "Fatal:" <+> text (toString msg)

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

instance Pretty Error where
  pretty Error {context = ErrorContext ctx, ..} =
    vcat ["While" <+> text (toString description) | AppContext {description} <- reverse ctx] $$
    pretty message
