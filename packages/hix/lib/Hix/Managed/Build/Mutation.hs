module Hix.Managed.Build.Mutation where

import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (VersionRange)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Bounds (BoundExtensions)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Overrides (Overrides)
import Hix.Data.Package (PackageName)
import Hix.Data.Version (NewRange, NewVersion, renderNewRange)

data DepMutation a =
  DepMutation {
    package :: PackageName,
    mutation :: a
  }
  deriving stock (Eq, Show, Generic)

instance Pretty a => Pretty (DepMutation a) where
  pretty DepMutation {package, mutation} = pretty package PrettyPrint.<> ":" <+> pretty mutation

data Candidate =
  Candidate {
    version :: NewVersion,
    range :: NewRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Candidate where
  pretty Candidate {..} =
    pretty version <+> brackets (renderNewRange range)

data BuildMutation =
  BuildMutation {
    candidate :: Candidate,
    newVersions :: [NewVersion],
    accOverrides :: Overrides,
    newBounds :: BoundExtensions
  }
  deriving stock (Eq, Show, Generic)

data MutationResult s =
  MutationSuccess Candidate ManagedState s
  |
  MutationUpdateBounds VersionRange
  |
  MutationKeep
  |
  MutationFailed
  deriving stock (Eq, Show, Generic)

class RenderMutation a where
  renderMutation :: DepMutation a -> Text
