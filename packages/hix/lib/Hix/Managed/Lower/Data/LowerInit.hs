module Hix.Managed.Lower.Data.LowerInit where

import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version, VersionRange)
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Version (showVersions)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation), RenderMutation (renderMutation))
import Hix.Managed.Data.SolverBounds (SolverBounds)
import Hix.Pretty (showP)

data LowerInit =
  LowerInit {
    versions :: NonEmpty Version,
    range :: VersionRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty LowerInit where
  pretty LowerInit {versions, range} =
    PrettyPrint.text (toString (showVersions versions)) <+> brackets (showP range)

instance RenderMutation LowerInit where
  renderMutation DepMutation {package, mutation = LowerInit {versions, range}} =
    [exon|##{package} #{showVersions versions} [#{showP range}]|]

data LowerInitState =
  LowerInitState {
    solverBounds :: SolverBounds
  }
  deriving stock (Eq, Show, Generic)
