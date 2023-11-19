module Hix.Managed.Build.Mutation where

import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (VersionRange)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint ((<+>))

import Hix.Data.Bounds (BoundExtensions)
import Hix.Data.ManagedEnv (ManagedState)
import Hix.Data.Overrides (Overrides)
import Hix.Data.Package (PackageName)
import Hix.Data.Version (NewVersion)
import Hix.Managed.Data.Candidate (Candidate)

data DepMutation a =
  DepMutation {
    package :: PackageName,
    mutation :: a
  }
  deriving stock (Eq, Show, Generic)

instance Pretty a => Pretty (DepMutation a) where
  pretty DepMutation {package, mutation} = pretty package PrettyPrint.<> ":" <+> pretty mutation

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
  MutationUpdateBounds NewVersion VersionRange
  |
  MutationKeep
  |
  MutationFailed
  deriving stock (Eq, Show, Generic)

class RenderMutation a where
  renderMutation :: DepMutation a -> Text