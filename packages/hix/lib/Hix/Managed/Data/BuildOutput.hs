module Hix.Managed.Data.BuildOutput where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.:?), (.=), Value (Object))
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Json (JsonParsec (JsonParsec))
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
    -- TODO why is this not named @bounds@? json?
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

instance FromJSON ModifiedId where
  parseJSON =
    withObject "ModifiedId" \ o -> do
      package <- o .: "package"
      JsonParsec version <- o .: "version"
      range <- o .:? "range"
      pure ModifiedId {..}

data DepChanges =
  DepChanges {
    modified :: [ModifiedId],
    unmodified :: [MutableDep],
    failed :: [MutableDep]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DepChangesNames =
  DepChangesNames {
    modifiedNames :: Maybe Text,
    unmodifiedNames :: Maybe Text,
    failedNames :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BuildOutput =
  BuildOutput {
    changes :: DepChanges,
    names :: DepChangesNames
  }
  deriving stock (Eq, Show)

instance FromJSON BuildOutput where
  parseJSON v = do
    changes <- parseJSON v
    names <- parseJSON v
    pure BuildOutput {..}

instance ToJSON BuildOutput where
  toJSON BuildOutput {..} =
    case (toJSON changes, toJSON names) of
      (Object c, Object n) -> Object (c <> n)
      _ -> Object [("error", "ToJSON BuildOutput invariant")]
