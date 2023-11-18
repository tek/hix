module Hix.Managed.Data.Candidate where

import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Version (NewRange, NewVersion, renderNewRange)

data Candidate =
  Candidate {
    version :: NewVersion,
    range :: NewRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Candidate where
  pretty Candidate {..} =
    pretty version <+> brackets (renderNewRange range)
