module Hix.Managed.Lower.Data.LowerOptimize where

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

data LowerOptimize =
  LowerOptimize {
    majors :: NonEmpty Major,
    range :: VersionRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty LowerOptimize where
  pretty LowerOptimize {majors, range} =
    PrettyPrint.text (toString (showMajors majors)) <+> brackets (showP range)

instance RenderMutation LowerOptimize where
  renderMutation DepMutation {package, mutation = LowerOptimize {majors, range}} =
    [exon|##{package} #{showMajors majors} [#{showP range}]|]

data LowerOptimizeState =
  LowerOptimizeState {
    solverBounds :: SolverBounds
  }
  deriving stock (Eq, Show, Generic)
