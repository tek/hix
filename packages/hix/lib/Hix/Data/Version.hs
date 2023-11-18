module Hix.Data.Version where

import Data.Aeson (FromJSON)
import qualified Data.List.NonEmpty as NonEmpty
import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange)
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import qualified Text.PrettyPrint as Pretty

import Hix.Data.Package (PackageName)
import Hix.Orphans.Version ()
import Hix.Pretty (showP)

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

forNewRange ::
  Applicative m =>
  NewRange ->
  (VersionRange -> m ()) ->
  m ()
forNewRange r f =
  traverse_ f (newRange r)

data NewVersion =
  NewVersion {
    package :: PackageName,
    version :: Version
  }
  deriving stock (Eq, Show, Generic)

renderNewVersion :: NewVersion -> Text
renderNewVersion NewVersion {..} =
  [exon|##{package}-#{showP version}|]

instance Pretty NewVersion where
  pretty = PrettyPrint.text . toString . renderNewVersion

renderNewVersionAsRange :: NewVersion -> Text
renderNewVersionAsRange NewVersion {..} =
  [exon|##{package} ==#{showP version}|]

showVersions :: NonEmpty Version -> Text
showVersions = \case
  [v] -> show (pretty v)
  versions -> [exon|#{show (pretty (NonEmpty.head versions))}-#{show (pretty (NonEmpty.last versions))}|]

newtype SourceHash =
  SourceHash Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (Ord, FromJSON)

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
