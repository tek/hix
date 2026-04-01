module Hix.Managed.Handlers.ReleaseUi where

import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage)
import Hix.Managed.Cabal.Data.UploadStage (UploadStage)
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Release.Data.ReleasePlan (ConfiguredTarget)
import Hix.Managed.Release.Data.SelectedVersion (SelectedVersion)
import Hix.Managed.Release.Data.Staged (PreparedTargetView, SelectedTargetView)
import Hix.Managed.Release.Data.TerminateFlow (TerminateFlow)
import Hix.Managed.Release.Data.VersionChoice (VersionChoice)
import Hix.Managed.Release.Validation (ProblematicVersion)

data ReleaseUi =
  ReleaseUi {
    -- | Present the new release versions determined from the flake and CLI config (if any) and allow the user to change
    -- them.
    -- Returns a 'VersionChoice' that preserves whether the user chose shared or individual versions.
    chooseVersions ::
      Maybe SelectedVersion ->
      Packages ConfiguredTarget ->
      M (Either TerminateFlow VersionChoice)
    ,

    -- | Present the results of an upload stage, prompting the selection of the packages with which to continue.
    -- Packages that failed should not be selectable.
    -- Returns 'Left' with a termination reason to stop the flow gracefully, or 'Right' with selected packages.
    chooseUploadTargets :: UploadStage -> Packages PreparedTargetView -> M (Either TerminateFlow (Set LocalPackage))
    ,

    -- | Present the result of running flake checks, prompting the selection of packages for which to create release
    -- distributions.
    -- If checks failed, the user can choose to force the release for selected packages.
    -- Returns 'Left' with a termination reason to stop the flow gracefully, or 'Right' with selected packages.
    chooseDistTargets :: Bool -> Packages SelectedTargetView -> M (Either TerminateFlow (Set LocalPackage))
    ,

    -- | Present version validation problems and ask user to confirm or abort.
    -- Returns 'Left' with a termination reason to stop the flow gracefully, or @Right True@ to continue, @Right False@ to abort.
    chooseVersionProblems :: Packages ProblematicVersion -> M (Either TerminateFlow Bool)
  }
