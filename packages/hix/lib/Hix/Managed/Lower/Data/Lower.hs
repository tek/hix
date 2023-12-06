module Hix.Managed.Lower.Data.Lower where

import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (VersionRange)
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Version (Major, showMajors)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation), RenderMutation (renderMutation))
import Hix.Managed.Data.SolverParams (SolverParams)
import Hix.Pretty (showP)

data Lower =
  Lower {
    majors :: NonEmpty Major,
    range :: VersionRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Lower where
  pretty Lower {majors, range} =
    PrettyPrint.text (toString (showMajors majors)) <+> brackets (showP range)

instance RenderMutation Lower where
  renderMutation DepMutation {package, mutation} =
    [exon|##{package} #{showP mutation}|]

data LowerState =
  LowerState {
    solverParams :: SolverParams
  }
  deriving stock (Eq, Show, Generic)
