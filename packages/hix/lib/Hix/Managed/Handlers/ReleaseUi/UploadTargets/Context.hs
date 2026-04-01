module Hix.Managed.Handlers.ReleaseUi.UploadTargets.Context where

import Control.Lens (ix)
import qualified Data.Functor as Functor
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Managed.Handlers.ReleaseUi.UploadTargets.State (
  UploadTarget (..),
  UploadTargetContext,
  UploadTargetScreen (..),
  )
import Hix.Managed.Release.Data.Staged (PreparedTargetView (..))
import Hix.Ui.Data.Nav (
  ActiveRow (..),
  Focusable (..),
  FocusableRow (..),
  Grid (..),
  NavMeta (..),
  Row (..),
  focusable,
  navContext,
  )

uiUploadTargets :: UploadStage -> NonEmpty (LocalPackage, PreparedTargetView) -> UploadTargetContext
uiUploadTargets stage packages =
  navContext screen navGrid
  where
    screen = UploadTargetScreen {stage, packages = Vector.fromList (toList states)}

    navGrid =
      Grid {
        pre = [],
        focus = FocusableRow {
          meta = meta0,
          row = OnlyRow
        },
        post = Seq.fromList (packageRow <$> toList metas)
      }

    (meta0 :| metas, states) = Functor.unzip (NonEmpty.zipWith packageData [1 ..] packages)

    packageRow meta = RowFocusable FocusableRow {meta, row = []}

    packageData index package =
      (packageMeta index, packageState package)

    packageMeta index =
      NavMeta {index, lens = #packages . ix index . #enabled}

    packageState (package, PreparedTargetView {releaseVersion}) =
      UploadTarget {enabled = focusable True, package, version = releaseVersion}

chosenPackages :: UploadTargetScreen -> Set LocalPackage
chosenPackages UploadTargetScreen {packages} =
  Set.fromList [package | UploadTarget {enabled, package} <- toList packages, enabled.state]
