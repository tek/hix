module Hix.Managed.Handlers.ReleaseUi.VersionProblems.State where

import Data.Vector (Vector)

import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Ui.Data.Nav (Focusable, NavContext)

-- | State for the version problems UI.
-- Shows problems with selected versions and allows user to confirm or abort.
data VersionProblemTarget =
  VersionProblemTarget {
    package :: LocalPackage,
    current :: Version,
    selected :: Version,
    problem :: Text
  }
  deriving stock (Eq, Show, Generic)

data VersionProblemsScreen =
  VersionProblemsScreen {
    -- | Whether the user accepts the problems and wants to continue.
    accepted :: Focusable Bool,
    problems :: Vector VersionProblemTarget
  }
  deriving stock (Eq, Show, Generic)

type VersionProblemsContext = NavContext VersionProblemsScreen Bool ()
