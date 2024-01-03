module Hix.Managed.Data.Diff where

import Data.These (These, mergeTheseWith)
import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (Doc, brackets, (<+>))

import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Pretty (prettyL)

class PrettyDiffDetail a where
  prettyDiffDetail :: a -> Maybe Doc

instance PrettyDiffDetail () where
  prettyDiffDetail () = Nothing

data Diff d a =
  DiffAdded a
  |
  DiffChanged {
    original :: a,
    new :: a,
    detail :: d
  }
  deriving stock (Eq, Show, Generic, Functor)

instance (
    Pretty a,
    PrettyDiffDetail d
  ) => Pretty (Diff d a) where
    pretty = \case
      DiffAdded a -> "[+]" <+> pretty a
      DiffChanged {original, new, detail} -> pretty original <+> "->" <+> pretty new <+> fold (prettyDiffDetail detail)

type VersionDiff = Diff () Version

newtype BoundsDiffDetail =
  BoundsDiffDetail (These VersionDiff VersionDiff)
  deriving stock (Eq, Show, Generic)

type BoundsDiff = Diff BoundsDiffDetail VersionBounds

instance PrettyDiffDetail BoundsDiffDetail where
  prettyDiffDetail (BoundsDiffDetail detail) =
    Just (brackets (mergeTheseWith pretty pretty cat detail))
    where
      cat l u = prettyL @[] [l, u]

data Change d a =
  Changed (Diff d a)
  |
  Unchanged (Maybe a)
  deriving stock (Eq, Show, Generic, Functor)

instance (
    Pretty a,
    PrettyDiffDetail d
  ) => Pretty (Change d a) where
    pretty = \case
      Unchanged Nothing -> "[0]"
      Unchanged (Just a) -> "[=]" <+> pretty a
      Changed diff -> pretty diff

type VersionChange = Change () Version

type BoundsChange = Change BoundsDiffDetail VersionBounds
