module Hix.Data.Version (
  module Hix.Data.Version,
  Version,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.List.NonEmpty as NonEmpty
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange, orLaterVersion, version0)
import Exon (exon)
import GHC.Exts (IsList)
import qualified Text.PrettyPrint as PrettyPrint
import qualified Text.PrettyPrint as Pretty

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMaybe, LookupMonoid, NtMap, ntPretty, ntPretty1)
import Hix.Data.EnvName (EnvName)
import Hix.Data.Json (jsonParsec)
import Hix.Data.PackageName (LocalPackage, PackageName)
import Hix.Orphans.Version ()
import Hix.Pretty (showP)

range0 :: VersionRange
range0 = orLaterVersion version0

data NewRange =
  NewRange VersionRange
  |
  OldRange
  deriving stock (Eq, Show, Generic)

renderNewRange :: IsString a => NewRange -> a
renderNewRange = \case
  NewRange r -> showP r
  OldRange -> "old range matches"

instance Pretty NewRange where
  pretty = PrettyPrint.text . renderNewRange

newRange :: NewRange -> Maybe VersionRange
newRange = \case
  NewRange r -> Just r
  OldRange -> Nothing

instance ToJSON NewRange where
  toJSON = toJSON @(Maybe Text) . fmap showP . newRange

instance FromJSON NewRange where
  parseJSON = fmap (maybe OldRange (NewRange . jsonParsec)) . parseJSON

forNewRange ::
  Applicative m =>
  NewRange ->
  (VersionRange -> m ()) ->
  m ()
forNewRange r f =
  traverse_ f (newRange r)

newtype SourceHash =
  SourceHash Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, FromJSON, EncodeNix)

instance Pretty SourceHash where
  pretty (SourceHash h) = Pretty.text (toString h)

data Major =
  Major {
    prefix :: Version,
    versions :: NonEmpty Version
  }
  deriving stock (Eq, Show, Generic)

showMajors :: NonEmpty Major -> Text
showMajors = \case
  [Major {prefix}] -> showP prefix
  majors -> [exon|#{showP (NonEmpty.head majors).prefix}-#{showP (NonEmpty.last majors).prefix}|]

newtype Versions =
  Versions (Map PackageName Version)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, EncodeNix)

instance NtMap Versions PackageName Version LookupMaybe where

instance Pretty Versions where
  pretty = ntPretty

instance FromJSON Versions where
  parseJSON v =
    Versions . fmap jsonParsec <$> parseJSON v

newtype TargetVersions =
  TargetVersions (Map LocalPackage Versions)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NtMap TargetVersions LocalPackage Versions LookupMonoid where

instance Pretty TargetVersions where
  pretty = ntPretty1

newtype EnvVersions =
  EnvVersions (Map EnvName Versions)
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, Semigroup, Monoid, IsList, EncodeNix)

instance NtMap EnvVersions EnvName Versions LookupMonoid where

instance Pretty EnvVersions where
  pretty = ntPretty1
