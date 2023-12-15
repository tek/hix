module Hix.Managed.Data.Candidate where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Json (jsonParsec)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.Version (NewRange, renderNewRange)
import Hix.Pretty (showP)

data Candidate =
  Candidate {
    package :: PackageId,
    range :: NewRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Candidate where
  pretty Candidate {..} =
    pretty package <+> brackets (renderNewRange range)

instance ToJSON Candidate where
  toJSON Candidate {..} =
    object [
      "name" .= toJSON package.name,
      "version" .= toJSON (showP @Text package.version),
      "range" .= toJSON range
    ]

instance FromJSON Candidate where
  parseJSON =
    withObject "Candidate" \ o -> do
      name <- o .: "name"
      version <- jsonParsec <$> o .: "version"
      range <- o .: "range"
      pure Candidate {package = PackageId {name, version}, range}
