module Hix.Managed.Handlers.ReleaseUi.DistTargets.Context where

import Control.Lens (ix)
import qualified Data.Functor as Functor
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Handlers.ReleaseUi.DistTargets.State (
  DistTarget (..),
  DistTargetContext,
  DistTargetScreen (..),
  )
import Hix.Managed.Release.Data.Staged (SelectedTargetView)
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

uiDistTargets :: Bool -> NonEmpty (LocalPackage, SelectedTargetView) -> DistTargetContext
uiDistTargets passed packages =
  navContext screen navGrid
  where
    screen = DistTargetScreen {checksPassed = passed, packages = Vector.fromList (toList states)}

    navGrid =
      Grid {
        pre = [],
        focus = FocusableRow {
          meta = meta0,
          row = OnlyRow
        },
        post = Seq.fromList (packageRow <$> toList metas)
      }

    (meta0 :| metas, states) = Functor.unzip (NonEmpty.zipWith packageData [0 ..] packages)

    packageRow meta = RowFocusable FocusableRow {meta, row = []}

    packageData index package =
      (packageMeta index, packageState package)

    packageMeta index =
      NavMeta {index, lens = #packages . ix index . #enabled}

    packageState (package, _target) =
      DistTarget {enabled = focusable True, package}

chosenPackages :: DistTargetScreen -> Set LocalPackage
chosenPackages DistTargetScreen {packages} =
  Set.fromList [package | DistTarget {enabled, package} <- toList packages, enabled.state]
