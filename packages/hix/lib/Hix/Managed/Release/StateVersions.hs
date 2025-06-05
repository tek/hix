module Hix.Managed.Release.StateVersions where

import Control.Lens ((%~))

import Hix.Class.Map (nAmend, nMap, nPad, nTransform)
import Hix.Data.Bounds (Bounds)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.Version (Version)
import qualified Hix.Data.VersionBounds as VersionBounds
import Hix.Managed.Data.PackageState (PackageState (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.StateVersionsContext (StateVersionsContext (..))
import Hix.Managed.Handlers.Context (ContextKey (ContextStateVersions), queryContext)
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import Hix.Managed.Handlers.Release (ReleaseHandlers (..))
import Hix.Managed.Release.Data.Staged (SelectedTargetView (..))
import Hix.Managed.Release.VersionFile (writeVersionFiles)
import Hix.Managed.StateFile (writeReleaseVersions)
import Hix.Monad (appContext)
import Hix.Version (nextMajor)

newPackageState :: SelectedTargetView -> PackageState
newPackageState SelectedTargetView {releaseVersion} =
  PackageState {version = Just releaseVersion}

updateVersion :: SelectedTargetView -> PackageState -> PackageState
updateVersion target PackageState {} =
  newPackageState target

updateUpperBounds :: Packages SelectedTargetView -> Packages Bounds -> Packages Bounds
updateUpperBounds targets =
  nMap (nAmend (<>) newBounds)
  where
    newBounds :: Bounds
    newBounds =
      flip nTransform targets \ (LocalPackage package) SelectedTargetView {releaseVersion} ->
        (package, VersionBounds.fromUpper (nextMajor releaseVersion))

updateStateVersions ::
  ReleaseHandlers ->
  Maybe Version ->
  Packages SelectedTargetView ->
  M ()
updateStateVersions handlers sharedVersion targets =
  appContext "updating the managed state to use the new versions" do
    context <- queryContext handlers.context ContextStateVersions
    let
      newState =
        context.state
        & #packages %~ nPad updateVersion newPackageState targets
        & #bounds %~ updateUpperBounds targets
    writeReleaseVersions handlers.project.stateFile newState
    writeVersionFiles sharedVersion context targets
