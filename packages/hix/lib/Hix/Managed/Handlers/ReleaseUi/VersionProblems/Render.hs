module Hix.Managed.Handlers.ReleaseUi.VersionProblems.Render where

import Brick (Padding (..), Widget, attrName, hBox, padBottom, padRight, txt, vBox, withAttr)

import Hix.Managed.Handlers.ReleaseUi.VersionProblems.State (VersionProblemTarget (..), VersionProblemsScreen (..))
import Hix.Pretty (showP)
import Hix.Ui (UiName)
import Hix.Ui.Data.Nav (Focusable (..))
import Hix.Ui.Render (toggleBulletAttr)

-- | Render the checkmark/cross bullet for acceptance state.
acceptBullet :: Bool -> Text
acceptBullet accepted =
  if accepted
  then "✓"
  else "✗"

renderProblem :: VersionProblemTarget -> Widget n
renderProblem VersionProblemTarget {..} =
  hBox [
    padRight (Pad 1) (withAttr (attrName "error") (txt "!")),
    txt (showP package),
    txt " ",
    withAttr (attrName "muted") (txt (showP current)),
    txt " → ",
    txt (showP selected),
    txt " - ",
    withAttr (attrName "warning") (txt problem)
  ]


renderAccepted :: Focusable Bool -> Widget n
renderAccepted Focusable {state, focused = _} =
  hBox [
    padRight (Pad 1) (withAttr (toggleBulletAttr state) (txt (acceptBullet state))),
    txt (if state then "Continue with problematic versions" else "Abort release")
  ]

render :: VersionProblemsScreen -> Widget UiName
render VersionProblemsScreen {problems, accepted} =
  vBox [
    vBox (renderProblem <$> toList problems),
    padBottom (Pad 1) (txt ""),
    renderAccepted accepted
  ]
