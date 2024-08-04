module Hix.Managed.Maint.Data.MaintPlan where

import Distribution.Pretty (Pretty (pretty))

import Hix.Class.Map (nPretty)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Maint.Data.MaintTarget (MaintTarget (..))

data MaintPlan =
  MaintPlan {
    targets :: Packages MaintTarget
  }
  deriving stock (Eq, Show)

instance Pretty MaintPlan where
  pretty MaintPlan {targets} = nPretty targets
