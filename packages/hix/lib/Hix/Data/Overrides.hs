module Hix.Data.Overrides where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.Version (Version)
import GHC.Exts (IsList)
import Text.PrettyPrint (brackets, (<+>))

import Hix.Class.Map (LookupMaybe, LookupMonoid, NtMap, ntPretty, ntPretty1, (!!))
import Hix.Data.EnvName (EnvName)
import Hix.Data.Json (JsonParsec (JsonParsec))
import Hix.Data.Package (PackageName)
import Hix.Data.Version (SourceHash)

data Override =
  Override {
    version :: Version,
    hash :: SourceHash
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Override where
  parseJSON =
    withObject "Override" \ o -> do
      JsonParsec version <- o .: "version"
      hash <- o .: "hash"
      pure Override {..}

instance Pretty Override where
  pretty Override {..} = pretty version <+> brackets (pretty hash)

newtype Overrides =
  Overrides (Map PackageName Override)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList)

instance NtMap Overrides PackageName Override LookupMaybe where

instance Pretty Overrides where
  pretty = ntPretty

newtype EnvOverrides =
  EnvOverrides (Map EnvName Overrides)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList)

instance NtMap EnvOverrides EnvName Overrides LookupMonoid where

instance Pretty EnvOverrides where
  pretty = ntPretty1

overrideVersionsFor :: Overrides -> Map PackageName Version
overrideVersionsFor (Overrides o) =
  o <&> \ Override {version} -> version

latestVersionNewer :: Overrides -> PackageName -> Version -> Bool
latestVersionNewer overrides package version =
  case overrides !! package of
    Nothing -> True
    Just Override {version = oVersion} -> oVersion < version

candidateMatchesOverride :: Overrides -> PackageName -> Version -> Bool
candidateMatchesOverride overrides package version =
  any match (overrides !! package)
  where
    match Override {version = oVersion} = oVersion == version
