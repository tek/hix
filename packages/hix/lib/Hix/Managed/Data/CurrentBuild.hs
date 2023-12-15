module Hix.Managed.Data.CurrentBuild where

import Hix.Data.PackageName (LocalPackage)

data CurrentBuild =
  CurrentBuild {
    target :: LocalPackage
  }
  deriving stock (Eq, Show, Generic)
