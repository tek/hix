module Hix.Managed.Handlers.ReleaseUi.VersionProblems.Context where

import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import Distribution.Pretty (pretty)
import Exon (exon)

import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Handlers.ReleaseUi.VersionProblems.State (VersionProblemTarget (..), VersionProblemsContext, VersionProblemsScreen (..))
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))
import Hix.Managed.Release.Validation (ProblematicVersion (..))
import Hix.Ui.Data.Nav (
  ActiveRow (..),
  Focusable (..),
  FocusableRow (..),
  Grid (..),
  NavMeta (..),
  navContext,
  )
import qualified Hix.Class.Map as Map

uiVersionProblems ::
  Packages ProblematicVersion ->
  VersionProblemsContext
uiVersionProblems problems =
  navContext screen navGrid
  where
    screen = VersionProblemsScreen {accepted, problems = targets}

    accepted = Focusable {state = False, focused = True}

    targets = Vector.fromList (fmap toTarget (Map.nElems problems))

    toTarget :: ProblematicVersion -> VersionProblemTarget
    toTarget ProblematicVersion {package, current, selected = SelectedVersion {version}, problem} =
      VersionProblemTarget {package, current, selected = version, problem = [exon|#{show (pretty problem)}|]}

    -- Simple grid with just the accept toggle
    navGrid =
      Grid {
        pre = [],
        focus = FocusableRow {
          meta = NavMeta {index = 0, lens = #accepted},
          row = OnlyRow
        },
        post = Seq.empty
      }

-- | Whether the user accepted the problems.
isAccepted :: VersionProblemsScreen -> Bool
isAccepted VersionProblemsScreen {accepted = Focusable {state}} = state

problematicPackages :: VersionProblemsScreen -> [LocalPackage]
problematicPackages VersionProblemsScreen {problems} =
  [p.package | p <- toList problems]
