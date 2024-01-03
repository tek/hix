module Hix.Managed.Data.BuildOutput where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Data.Mutable (MutableDep)
import qualified Hix.Managed.Data.MutableId
import Hix.Managed.Data.MutableId (MutableId (MutableId))
import Hix.Pretty (showP)

data ModifiedId =
  ModifiedId {
    package :: MutableDep,
    version :: Version,
    range :: Maybe VersionBounds
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ModifiedId where
  pretty ModifiedId {..} =
    pretty MutableId {name = package, version} <+> brackets (maybe "old range matches" pretty range)

instance ToJSON ModifiedId where
  toJSON ModifiedId {..} =
    object [
      "package" .= toJSON package,
      "version" .= toJSON (showP version :: Text),
      "range" .= toJSON (showP <$> range :: Maybe Text)
    ]

data BuildOutput =
  BuildOutput {
    modified :: [ModifiedId],
    unmodified :: [MutableDep],
    failed :: [MutableDep],
    modifiedNames :: Maybe Text,
    unmodifiedNames :: Maybe Text,
    failedNames :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
