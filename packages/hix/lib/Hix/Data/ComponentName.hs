module Hix.Data.ComponentName where

import Data.Aeson (FromJSON, FromJSONKey)
import Distribution.Pretty (Pretty (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

import Hix.Pretty (prettyText)

newtype ComponentName =
  ComponentName { unComponentName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey)

instance Pretty ComponentName where
  pretty (ComponentName n) = prettyText n

fromCabal :: LibraryName -> ComponentName
fromCabal = \case
  LMainLibName -> "library"
  LSubLibName name -> fromString (unUnqualComponentName name)
