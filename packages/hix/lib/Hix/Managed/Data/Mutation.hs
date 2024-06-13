module Hix.Managed.Data.Mutation where

import Distribution.Pretty (Pretty (pretty))
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (parens, (<+>))

import Hix.Data.PackageId (PackageId)
import Hix.Data.Version (Version)
import Hix.Data.VersionBounds (VersionBounds)
import Hix.Managed.Cabal.Data.SolverState (SolverState)
import Hix.Managed.Data.Mutable (MutableDep)
import Hix.Managed.Data.MutableId (MutableId)
import Hix.Managed.Data.MutationState (MutationState)
import Hix.Pretty (prettyText, showP)

data DepMutation a =
  DepMutation {
    package :: MutableDep,
    retract :: Bool,
    mutation :: a
  }
  deriving stock (Eq, Show, Generic)

instance Pretty a => Pretty (DepMutation a) where
  pretty DepMutation {package, mutation, retract} =
    pretty package PrettyPrint.<> ":" <+> pretty mutation <+> parens retext
    where
      retext | retract = "retract"
             | otherwise = "extend"

data BuildMutation =
  BuildMutation {
    description :: Text,
    solverState :: SolverState,
    updateBound :: Version -> VersionBounds -> VersionBounds
  }
  deriving stock (Generic)

data MutationResult s =
  MutationSuccess {
    candidate :: MutableId,
    changed :: Bool,
    state :: MutationState,
    revisions :: Set PackageId,
    ext :: s
  }
  |
  MutationKeep
  |
  MutationFailed
  deriving stock (Eq, Show, Generic)

data FailedMutation =
  FailedMutation {
    package :: MutableDep,
    mutation :: Text
  }
  deriving stock (Eq, Show, Generic)

instance Pretty FailedMutation where
  pretty FailedMutation {package, mutation} = pretty package <+> prettyText mutation

failedMutation ::
  Pretty a =>
  DepMutation a ->
  FailedMutation
failedMutation DepMutation {package, mutation} =
  FailedMutation {package, mutation = showP mutation}
