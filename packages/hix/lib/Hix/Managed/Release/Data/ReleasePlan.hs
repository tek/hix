module Hix.Managed.Release.Data.ReleasePlan where

import Distribution.Pretty (Pretty (pretty))
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Version (Version)
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseTarget)
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))

data ConfiguredTarget =
  ConfiguredTarget {
    current :: Version,
    version :: Maybe SelectedVersion,
    selected :: Bool
  }
  deriving stock (Eq, Show)

instance Pretty ConfiguredTarget where
  pretty ConfiguredTarget {..} =
    pretty current <+> foldMap change version <+> brackets (if selected then "x" else " ")
    where
      change v = "->" <+> pretty v.version

data ReleasePlan =
  Planned ReleaseTarget
  |
  NotPlanned ConfiguredTarget
  deriving stock (Eq, Show)
