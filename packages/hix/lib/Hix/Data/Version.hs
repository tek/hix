module Hix.Data.Version (
  module Hix.Data.Version,
  Version,
  VersionRange,
) where

import Data.Aeson (FromJSON (parseJSON))
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NonEmpty
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange, anyVersion)
import Exon (exon)
import GHC.Exts (IsList)
import Text.PrettyPrint (Doc)

import Hix.Class.EncodeNix (EncodeNix)
import Hix.Class.Map (LookupMaybe, NMap, nGenWith, nPretty)
import Hix.Data.Json (jsonParsec)
import qualified Hix.Data.PackageId
import Hix.Data.PackageId (PackageId (PackageId))
import Hix.Data.PackageName (PackageName)
import Hix.Orphans.Version ()
import Hix.Pretty (prettyText)

range0 :: VersionRange
range0 = anyVersion

newtype SourceHash =
  SourceHash Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, FromJSON, EncodeNix)

instance Pretty SourceHash where
  pretty (SourceHash h) = prettyText h

data Major =
  Major {
    prefix :: Version,
    versions :: NonEmpty Version
  }
  deriving stock (Eq, Show, Generic)

prettyMajors :: NonEmpty Major -> Doc
prettyMajors = \case
  [Major {prefix}] -> pretty prefix <> ".*"
  majors -> [exon|#{pretty (NonEmpty.head majors).prefix}–#{pretty (NonEmpty.last majors).prefix}|]

newtype Versions =
  Versions (Map PackageName Version)
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid, IsList, EncodeNix)

instance NMap Versions PackageName Version LookupMaybe where

instance Pretty Versions where
  pretty = nPretty

instance FromJSON Versions where
  parseJSON v =
    Versions . fmap jsonParsec <$> parseJSON v

packageIdVersions :: [PackageId] -> Versions
packageIdVersions =
  nGenWith \ PackageId {name, version} -> (name, version)
