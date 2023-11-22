module Hix.Managed.Data.Candidate where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import qualified Hix.Data.Version
import Hix.Data.Version (NewRange, NewVersion, renderNewRange)
import Hix.Pretty (showP)

data Candidate =
  Candidate {
    version :: NewVersion,
    range :: NewRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Candidate where
  pretty Candidate {..} =
    pretty version <+> brackets (renderNewRange range)

instance ToJSON Candidate where
  toJSON Candidate {..} =
    object [
      "package" .= toJSON version.package,
      "version" .= toJSON (showP @Text version.version),
      "range" .= toJSON range
    ]
