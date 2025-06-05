module Hix.Integration.TuiTest where

import Hix.Data.Monad (M)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi (ReleaseUi (..))
import qualified Hix.Managed.Handlers.ReleaseUi.Prod as ReleaseUi
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..))
import Hix.Managed.Release.Data.SelectedVersion (explicitVersion)
import Hix.Test.Managed.UnsafeIsString ()

packages :: Packages ConfiguredTarget
packages =
  [
    ("local1", target "1.0.1"),
    ("local2", target "1.0.2"),
    ("local3", target "1.0.3"),
    ("local4", target "1.0.4"),
    ("local5", target "1")
  ]
  where
    target current = ConfiguredTarget {current, version = Nothing, selected = True}

tuiTest :: M ()
tuiTest = do
  result <- ReleaseUi.handlersProd.chooseVersions (Just (explicitVersion "1.1.0")) packages
  dbgs result
