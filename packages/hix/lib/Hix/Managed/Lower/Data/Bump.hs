module Hix.Managed.Lower.Data.Bump where

import Distribution.Pretty (Pretty (pretty))
import Distribution.Version (Version)
import Exon (exon)
import qualified Text.PrettyPrint as PrettyPrint
import Text.PrettyPrint (brackets, (<+>))

import Hix.Data.Overrides (Overrides)
import Hix.Data.Version (NewRange, renderNewRange)
import qualified Hix.Managed.Build.Mutation
import Hix.Managed.Build.Mutation (DepMutation (DepMutation), RenderMutation (renderMutation))
import Hix.Pretty (showP)

data Bump =
  Bump {
    version :: Version,
    range :: NewRange
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Bump where
  pretty Bump {version, range} =
    pretty version <+> brackets (PrettyPrint.text (renderNewRange range))

instance RenderMutation Bump where
  renderMutation DepMutation {package, mutation = Bump {version, range}} =
    [exon|##{package} #{showP version} [#{renderNewRange range}]|]

data BumpState =
  BumpState {
    overrides :: Overrides
  }
  deriving stock (Eq, Show, Generic)
