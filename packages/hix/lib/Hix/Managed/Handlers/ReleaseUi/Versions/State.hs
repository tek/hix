module Hix.Managed.Handlers.ReleaseUi.Versions.State where

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Ui.Data.Nav (Focusable (..), NavContext)

data Components =
  Components {
    super :: Focusable Int,
    subs :: Vector (Focusable Int)
  }
  deriving stock (Eq, Show, Generic)

componentsFocused :: Components -> Bool
componentsFocused c =
  c.super.focused || any (.focused) c.subs

data TogglableVersion =
  TogglableVersion {
    enabled :: Bool,
    components :: Components
  }
  deriving stock (Eq, Show, Generic)

withNonEmptyComponents ::
  (NonEmpty (Focusable Int) -> NonEmpty (Focusable Int)) ->
  TogglableVersion ->
  TogglableVersion
withNonEmptyComponents f TogglableVersion {enabled, components = Components {..}} =
  TogglableVersion {enabled, components = Components {super = h, subs = Vector.fromList t}}
  where
    h :| t = f (super :| toList subs)

data PackageVersion =
  PackageVersion {
    package :: LocalPackage,
    current :: Version,
    version :: Focusable TogglableVersion
  }
  deriving stock (Eq, Show, Generic)

packageVersionEnabled :: PackageVersion -> Bool
packageVersionEnabled PackageVersion {version = Focusable {state = TogglableVersion {enabled}}} =
  enabled

data VersionsScreen =
  VersionsScreen {
    shared :: Focusable TogglableVersion,
    packages :: Vector PackageVersion
  }
  deriving stock (Eq, Show, Generic)

type VersionsContext = NavContext VersionsScreen TogglableVersion Int
