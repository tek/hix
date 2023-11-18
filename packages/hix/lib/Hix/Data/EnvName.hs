module Hix.Data.EnvName where

import Data.Aeson (FromJSON, FromJSONKey)
import Distribution.Pretty (Pretty (pretty))
import qualified Text.PrettyPrint as Pretty

newtype EnvName =
  EnvName { unEnvName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, FromJSON, FromJSONKey)

instance Pretty EnvName where
  pretty (EnvName n) = Pretty.text (toString n)
