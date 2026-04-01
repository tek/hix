module Hix.Managed.Handlers.ReleaseUi.DistTargets.State where

import Data.Vector (Vector)
import Distribution.Version (Version)

import Hix.Data.PackageName (LocalPackage)
import Hix.Ui.Data.Nav (Focusable, NavContext)

-- | State for the dist targets UI.
-- Shows whether global flake checks passed and allows selecting packages to continue with.
data DistTarget =
  DistTarget {
    enabled :: Focusable Bool,
    package :: LocalPackage,
    version :: Version
  }
  deriving stock (Eq, Show, Generic)

data DistTargetScreen =
  DistTargetScreen {
    checksPassed :: Bool,
    packages :: Vector DistTarget
  }
  deriving stock (Eq, Show, Generic)

type DistTargetContext = NavContext DistTargetScreen Bool ()
