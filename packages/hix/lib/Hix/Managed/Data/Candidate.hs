module Hix.Managed.Data.Candidate where

import Data.Aeson (ToJSON (toJSON), object, (.=), FromJSON (parseJSON), withObject, (.:))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import qualified Hix.Data.Version
import Hix.Data.Version (NewRange, NewVersion (NewVersion), renderNewRange)
import Hix.Pretty (showP)
import Hix.Data.Json (jsonParsec)

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

instance FromJSON Candidate where
  parseJSON =
    withObject "Candidate" \ o -> do
      package <- o .: "package"
      version <- jsonParsec <$> o .: "version"
      range <- o .: "range"
      pure Candidate {version = NewVersion {package, version}, range}
