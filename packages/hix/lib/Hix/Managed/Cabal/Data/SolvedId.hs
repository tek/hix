module Hix.Managed.Cabal.Data.SolvedId where

import Distribution.Pretty (Pretty (..))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Overrides (IsRevision (..))
import Hix.Data.PackageId (PackageId)

data SolvedId =
  SolvedId {
    package :: PackageId,
    revision :: Maybe IsRevision
  }
  deriving stock (Eq, Show)

instance Pretty SolvedId where
  pretty SolvedId {..} =
    pretty package <+> foldMap renderRevision revision
    where
      renderRevision = \case
        IsRevision -> brackets "rev"
        IsNotRevision -> ""

revisedId :: PackageId -> SolvedId
revisedId package = SolvedId {package, revision = Just IsRevision}

unrevisedId :: PackageId -> SolvedId
unrevisedId package = SolvedId {package, revision = Nothing}
