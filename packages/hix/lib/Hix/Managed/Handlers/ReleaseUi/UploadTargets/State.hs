module Hix.Managed.Handlers.ReleaseUi.UploadTargets.State where

import Data.Vector (Vector)

import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Ui.Data.Nav (Focusable, NavContext)

data UploadTarget =
  UploadTarget {
    enabled :: Focusable Bool,
    package :: LocalPackage
  }
  deriving stock (Eq, Show, Generic)

data UploadTargetScreen =
  UploadTargetScreen {
    stage :: UploadStage,
    packages :: Vector UploadTarget
  }
  deriving stock (Eq, Show, Generic)

type UploadTargetContext = NavContext UploadTargetScreen Bool ()
