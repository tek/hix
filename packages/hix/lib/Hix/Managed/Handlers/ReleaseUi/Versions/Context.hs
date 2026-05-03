module Hix.Managed.Handlers.ReleaseUi.Versions.Context where

import Control.Lens (Traversal', ix, (^.))
import Data.Functor.WithIndex (imap)
import Data.List (findIndex)
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
  packageVersionEnabled,
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
import Hix.Ui.Nav (focusRow, gridNavigateToRow)

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
versionTarget package ConfiguredTarget {current, version = configuredVersion, selected} =
  PackageVersion {
    package,
    current,
    version = focusable TogglableVersion {enabled = selected, components}
  }
  where
    components = versionComponents (maybe current (.version) configuredVersion)

-- TODO with @showShared@ now having to be synced with other code, it would be better to abstract over shared/individual
-- somewhere up the call graph.
uiVersions ::
  Maybe SelectedVersion ->
  Packages ConfiguredTarget ->
  VersionsContext
uiVersions configuredSharedVersion targets =
  navContext screen initialGrid
  where
    initialGrid =
      if startIndividual
      then fromMaybe baseGrid (gridNavigateToRow firstEnabledRow 0 baseGrid)
      else baseGrid

    baseGrid =
      Grid {
        pre = [],
        focus = active 0,
        post = Seq.fromList [RowFocusable (f i) | (f, i) <- zip rows [1 ..]]
      }

    (active, rows) = if showShared then withShared else withoutShared

    -- | Start in individual mode when shared row is shown but not all packages are enabled.
    startIndividual = showShared && not allEnabled

    allEnabled = all packageVersionEnabled packages

    -- | Row index of the first enabled package (1-based, since shared row is 0).
    firstEnabledRow = fromMaybe 0 (findIndex packageVersionEnabled packages) + 1

    withShared = (sharedRow, componentRow <$> [0 .. length packages - 1])

    withoutShared = (fmap (focusRow 1) . componentRow 0, (componentRow <$> [1 .. length packages - 1]))

    sharedRow index =
      FocusableRow {
        meta = NavMeta {index, lens = sharedLens},
        row = ActiveRow {
          pre = [TileUnfocusable],
          focus = NavMeta {index = 0, lens = sharedLens . #state . #components . #super},
          post = fromList (componentList sharedLens)
        }
      }

    componentRow lensIndex rowIndex =
      let
        lens :: Traversal' VersionsScreen (Focusable TogglableVersion)
        lens = #packages . ix lensIndex . #version
      in FocusableRow {
        meta = NavMeta {index = rowIndex, lens},
        row = [
          TileUnfocusable,
          TileFocusable NavMeta {index = 0, lens = lens . #state . #components . #super}
        ] <> fromList (toList (imap (tailComponent lens) (screen ^. (lens . #state . #components . #subs))))
      }

    -- | Show shared row when there are multiple selected packages
    showShared = length packages > 1

    sharedLens :: Traversal' VersionsScreen (Focusable TogglableVersion)
    sharedLens = #shared

    componentList (l :: Traversal' VersionsScreen (Focusable TogglableVersion)) =
      toList (imap (tailComponent l) (screen ^. (l . #state . #components . #subs)))

    tailComponent (l :: Traversal' VersionsScreen (Focusable TogglableVersion)) i _ =
      TileFocusable NavMeta {index = i + 1, lens = l . #state . #components . #subs . ix i}

    screen = VersionsScreen {
      shared = focusable TogglableVersion {
        enabled = isJust configuredSharedVersion && allEnabled,
        components = sharedVersion
      },
      packages = Vector.fromList packages
    }

    sharedVersion = versionComponents (maybe [0] (.version) configuredSharedVersion)

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
