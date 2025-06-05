module Hix.Managed.Release.Validation where

import Distribution.Pretty (Pretty (pretty))
import Exon (exon)
import Text.PrettyPrint ((<+>))

import Hix.Class.Map (nForWithKey_, nMapMaybe, nNull)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Version)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.ReleaseConfig (ReleaseConfig (..))
import Hix.Managed.Data.VersionIncrement (VersionIncrement (..))
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion (..))
import Hix.Managed.Release.Data.Staged (SelectedTargetView (..))
import Hix.Managed.VersionIncrement (incrementVersion)
import Hix.Monad (clientError)
import Hix.Maybe (justIf)

-- | A problem with a selected version that prevents release.
data VersionProblem =
  -- | No version or increment was specified.
  NoVersionSpecified
  |
  -- | The selected version equals the current version but wasn't explicitly kept.
  SameAsCurrent
  |
  -- | The selected version is smaller than the current version.
  SmallerThanCurrent
  |
  -- | The selected version is more than one increment larger than the current version.
  TooLargeIncrement
  deriving stock (Eq, Show, Generic)

instance Pretty VersionProblem where
  pretty = \case
    NoVersionSpecified -> "no version specified"
    SameAsCurrent -> "same as current version"
    SmallerThanCurrent -> "smaller than current version"
    TooLargeIncrement -> "more than one increment larger"

-- | Details about a problematic version for display in the UI.
data ProblematicVersion =
  ProblematicVersion {
    package :: LocalPackage,
    current :: Version,
    selected :: SelectedVersion,
    problem :: VersionProblem
  }
  deriving stock (Eq, Show, Generic)

instance Pretty ProblematicVersion where
  pretty ProblematicVersion {..} =
    pretty package <+> "|" <+> pretty current <+> "->" <+> pretty selected <+> "|" <+> pretty problem

-- | Check if a selected version is valid for release.
-- Returns 'Nothing' if valid, or the problem if invalid.
--
-- @keep@ only bypasses validation when @version@ equals @current@, enforcing the invariant that @keep@ means
-- "use current version".
validateVersion :: Version -> SelectedVersion -> Maybe VersionProblem
validateVersion current SelectedVersion {version, explicit, keep}
  | keep && version == current =
    -- Explicitly kept with matching version: valid
    Nothing
  | not explicit =
    -- Not explicit: must have been auto-selected, which shouldn't happen if no version was provided and no increment
    -- was applied
      Just NoVersionSpecified
  | version == current =
    Just SameAsCurrent
  | version < current =
    Just SmallerThanCurrent
  | isLargeIncrement current version =
    Just TooLargeIncrement
  | otherwise =
    Nothing

-- | Check if the version is more than one increment larger than the current.
-- This catches cases like jumping from 1.1.2 to 3.0.0.
isLargeIncrement :: Version -> Version -> Bool
isLargeIncrement current new =
  new > supermajorBump && new > majorBump && new > minorBump && new > patchBump
  where
    supermajorBump = incrementVersion current Supermajor
    majorBump = incrementVersion current Major
    minorBump = incrementVersion current Minor
    patchBump = incrementVersion current Patch

-- | Find all problematic versions in the release plan.
findProblematicVersions :: Packages SelectedTargetView -> Packages ProblematicVersion
findProblematicVersions =
  nMapMaybe \ SelectedTargetView {package, current, releaseVersion, explicit, keep} ->
    let selected = SelectedVersion {version = releaseVersion, explicit, keep}
    in validateVersion current selected <&> \ problem ->
      ProblematicVersion {package, current, selected, problem}

-- | Run version validation.
-- If @forceVersion@ is set, skip validation.
-- In interactive mode, return problems for UI display.
-- In batch mode, abort with error if there are problems.
validateVersions ::
  ReleaseConfig ->
  Packages SelectedTargetView ->
  Maybe (Packages ProblematicVersion)
validateVersions config targets
  | config.forceVersion =
      -- Force mode: skip validation
      Nothing
  | otherwise =
      let problems = findProblematicVersions targets
      in justIf (not (nNull problems)) problems

-- | Batch mode validation: abort if there are problems.
validateVersionsBatch ::
  Packages ProblematicVersion ->
  M ()
validateVersionsBatch problems =
  nForWithKey_ problems \ _ prob ->
    clientError [exon|Version validation failed for #{show (pretty prob)}. Use --force-version to bypass.|]
