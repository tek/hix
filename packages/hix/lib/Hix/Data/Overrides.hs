module Hix.Data.Overrides where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.Version (Version)
import GHC.Exts (IsList)
import Text.PrettyPrint (brackets, (<+>))

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMaybe, NMap, nPretty)
import Hix.Data.Json (JsonParsec (JsonParsec))
import Hix.Data.PackageName (PackageName)
import Hix.Data.Version (SourceHash)

data Override =
  Override {
    version :: Version,
    hash :: SourceHash
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (EncodeNix)

instance FromJSON Override where
  parseJSON =
    withObject "Override" \ o -> do
      JsonParsec version <- o .: "version"
      hash <- o .: "hash"
      pure Override {..}

instance Pretty Override where
  pretty Override {..} = pretty version <+> brackets (pretty hash)

-- | Overrides can be either for mutable (direct, nonlocal) deps, or for transitive deps, so they must use
-- 'PackageName'.
newtype Overrides =
  Overrides (Map PackageName Override)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NMap Overrides PackageName Override LookupMaybe where

instance Pretty Overrides where
  pretty = nPretty
