module Hix.Managed.Data.Bump where

import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, parens, (<+>))

import Hix.Data.Version (Version)

data Bump =
  Bump {
    version :: Version,
    bound :: Version,
    changed :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Bump where
  pretty Bump {version, bound, changed} =
    pretty version <+> parens ("<" <> pretty bound) <+> brackets (if changed then "x" else " ")
