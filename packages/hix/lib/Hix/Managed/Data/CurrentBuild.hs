module Hix.Managed.Data.CurrentBuild where

import Hix.Data.Package (LocalPackage)

data CurrentBuild =
  CurrentBuild {
    target :: LocalPackage
  }
  deriving stock (Eq, Show, Generic)
