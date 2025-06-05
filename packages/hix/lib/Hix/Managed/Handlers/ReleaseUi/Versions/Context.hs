module Hix.Managed.Handlers.ReleaseUi.Versions.Context where

import Control.Lens (Traversal', ix, (^.))
import Data.Functor.WithIndex (imap)
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import Distribution.Version (mkVersion, versionNumbers)
import GHC.Exts (fromList)

import Hix.Class.Map (nGenMaybe, nList)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi.Versions.State (
  Components (..),
  PackageVersion (..),
  TogglableVersion (..),
  VersionsContext,
  VersionsScreen (..),
  )
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget (..))
import Hix.Managed.Release.Data.ReleaseTarget (ReleaseTarget (..))
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..), explicitVersion, keepVersion)
import Hix.Managed.Release.Data.VersionChoice (SharedTarget (..), VersionChoice (..))
import Hix.Ui.Data.Nav (
  ActiveRow (..),
  Focusable (..),
  FocusableRow (..),
  Grid (..),
  NavMeta (..),
  Row (..),
  Tile (..),
  focusable,
  navContext,
  )

versionComponents ::
  Version ->
  Components
versionComponents current =
  Components {
    super = focusable firstComponent,
    subs = focusable <$> Vector.fromList tailComponents
  }
  where
    (firstComponent, tailComponents) = fromMaybe (0, [1, 0]) (uncons (versionNumbers current))

versionTarget ::
  LocalPackage ->
  ConfiguredTarget ->
  PackageVersion
versionTarget package ConfiguredTarget {current, selected} =
  PackageVersion {
    package,
    current,
    version = focusable TogglableVersion {enabled = selected, components}
  }
  where
    components = versionComponents current

uiVersions ::
  Maybe SelectedVersion ->
  Packages ConfiguredTarget ->
  VersionsContext
uiVersions configuredSharedVersion targets =
  navContext screen navGrid
  where
    navGrid =
      Grid {
        pre = [],
        focus = FocusableRow {
          meta = NavMeta {index = 0, lens = sharedLens},
          row = ActiveRow {
            pre = [TileUnfocusable],
            focus = NavMeta {index = 0, lens = sharedLens . #state . #components . #super},
            post = fromList (componentList sharedLens)
          }
        },
        post = componentRow <$> Seq.fromList [0 .. length packages - 1]
      }

    sharedLens :: Traversal' VersionsScreen (Focusable TogglableVersion)
    sharedLens = #shared

    componentList (l :: Traversal' VersionsScreen (Focusable TogglableVersion)) =
      toList (imap (tailComponent l) (screen ^. (l . #state . #components . #subs)))

    componentRow i = vRow i (#packages . ix i . #version)

    vRow i (l :: Traversal' VersionsScreen (Focusable TogglableVersion)) =
      RowFocusable FocusableRow {
        meta = NavMeta {index = i + 1, lens = l},
        row = [
          TileUnfocusable,
          TileFocusable NavMeta {index = 0, lens = l . #state . #components . #super}
        ] <> fromList (toList (imap (tailComponent l) (screen ^. (l . #state . #components . #subs))))
      }

    tailComponent (l :: Traversal' VersionsScreen (Focusable TogglableVersion)) i _ =
      TileFocusable NavMeta {index = i + 1, lens = l . #state . #components . #subs . ix i}

    screen = VersionsScreen {
      shared = focusable TogglableVersion {
        enabled = isJust configuredSharedVersion,
        components = sharedVersion
      },
      packages = Vector.fromList packages
    }

    sharedVersion = versionComponents (maybe [0] (\sv -> sv.version) configuredSharedVersion)

    packages = uncurry versionTarget <$> nList targets

-- | Extract the user's version choice from the UI state.
-- If shared version is enabled, returns 'SharedVersion' and targets without individual versions.
-- Otherwise, returns 'IndividualVersions' with per-package versions.
chosenVersions ::
  VersionsScreen ->
  VersionChoice
chosenVersions VersionsScreen {shared, packages} =
  if shared.state.enabled
  then SharedVersion {
    version = sharedVersionValue,
    sharedTargets = nGenMaybe packages toSharedTarget,
    sharedExcluded = nGenMaybe packages toExcluded
  }
  else IndividualVersions {
    individualTargets = nGenMaybe packages toIndividualTarget,
    individualExcluded = nGenMaybe packages toExcluded
  }
  where
    sharedVersionValue =
      let newVersion = mkVersion [c.state | c <- shared.state.components.super : toList shared.state.components.subs]
      in explicitVersion newVersion

    toSharedTarget :: PackageVersion -> Maybe (LocalPackage, SharedTarget)
    toSharedTarget PackageVersion {package, current, version = Focusable {state = TogglableVersion {enabled}}}
      | enabled = Just (package, SharedTarget {package, current})
      | otherwise = Nothing

    toIndividualTarget :: PackageVersion -> Maybe (LocalPackage, ReleaseTarget)
    toIndividualTarget PackageVersion {package, current, version = Focusable {state = TogglableVersion {enabled, components}}}
      | enabled =
        let
          newVersion = mkVersion [c.state | c <- components.super : toList components.subs]
          -- If version equals current, treat as explicit keep; otherwise explicit version
          selectedVersion
            | newVersion == current = keepVersion current
            | otherwise = explicitVersion newVersion
        in Just (package, ReleaseTarget {package, current, version = selectedVersion})
      | otherwise = Nothing

    toExcluded :: PackageVersion -> Maybe (LocalPackage, ConfiguredTarget)
    toExcluded PackageVersion {package, current, version = Focusable {state = TogglableVersion {enabled}}}
      | not enabled = Just (package, ConfiguredTarget {current, version = Nothing, selected = False})
      | otherwise = Nothing
