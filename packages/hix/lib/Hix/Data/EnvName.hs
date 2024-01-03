module Hix.Data.EnvName where

import Data.Aeson (FromJSON, FromJSONKey)
import Distribution.Pretty (Pretty (pretty))

import Hix.Class.EncodeNix (EncodeNixKey)
import Hix.Pretty (prettyText)

newtype EnvName =
  EnvName { unEnvName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey, EncodeNixKey)

instance Pretty EnvName where
  pretty (EnvName n) = prettyText n
