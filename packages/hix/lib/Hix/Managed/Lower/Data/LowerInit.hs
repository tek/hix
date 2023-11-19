module Hix.Managed.Lower.Data.LowerInit where

import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (VersionRange)
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Version (Major, showMajors)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation), RenderMutation (renderMutation))
import Hix.Managed.Data.SolverBounds (SolverBounds)
import Hix.Pretty (showP)

data LowerInit =
  LowerInit {
    majors :: NonEmpty Major,
    range :: VersionRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty LowerInit where
  pretty LowerInit {majors, range} =
    PrettyPrint.text (toString (showMajors majors)) <+> brackets (showP range)

instance RenderMutation LowerInit where
  renderMutation DepMutation {package, mutation = LowerInit {majors, range}} =
    [exon|##{package} #{showMajors majors} [#{showP range}]|]

data LowerInitState =
  LowerInitState {
    solverBounds :: SolverBounds
  }
  deriving stock (Eq, Show, Generic)
