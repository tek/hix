module Hix.Managed.Data.Lower where

import Distribution.Pretty (Pretty (pretty))

import Hix.Data.Version (Major, prettyMajors)

data Lower =
  Lower {
    majors :: NonEmpty Major
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Lower where
  pretty Lower {majors} = prettyMajors majors
