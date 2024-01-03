module Hix.Data.Bounds where

import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange)
import GHC.Exts (IsList)

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMaybe, NMap, nPretty)
import Hix.Data.Json (jsonParsec)
import Hix.Data.PackageName (PackageName)
import Hix.Data.VersionBounds (VersionBounds)

newtype Bounds =
  Bounds (Map PackageName VersionBounds)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, EncodeNix, FromJSON, ToJSON)

instance NMap Bounds PackageName VersionBounds LookupMaybe where

instance Pretty Bounds where
  pretty = nPretty

newtype Ranges =
  Ranges (Map PackageName VersionRange)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap Ranges PackageName VersionRange LookupMaybe where

instance Pretty Ranges where
  pretty = nPretty

instance FromJSON Ranges where
  parseJSON v =
    Ranges . fmap jsonParsec <$> parseJSON v

data BoundExtension =
  LowerBoundExtension Version
  |
  UpperBoundExtension Version
  deriving stock (Eq, Show, Generic)

newtype BoundExtensions =
  BoundExtensions (Map PackageName BoundExtension)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList)

instance NMap BoundExtensions PackageName BoundExtension LookupMaybe where
