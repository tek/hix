module Hix.Managed.Release.StateVersions where

import Control.Lens ((%~))
import qualified Data.Map.Strict as Map
import Exon (exon)

import Hix.Class.Map (nAmend, nGet, nKeys, nMap, nPad, nTransform)
import qualified Hix.Color as Color
import Hix.Data.Bounds (Bounds)
import Hix.Data.Monad (M)
import Hix.Data.PackageName (LocalPackage (..))
import Hix.Data.Version (Version)
import qualified Hix.Data.VersionBounds as VersionBounds
import qualified Hix.Log as Log
import Hix.Managed.Data.PackageState (PackageState (..))
import Hix.Managed.Data.Packages (Packages)
import Hix.Managed.Data.StateVersionsContext (StateVersionsContext (..), StateVersionsPackage (..))
import Hix.Managed.Handlers.Context (ContextKey (ContextStateVersions), queryContext)
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import Hix.Managed.Handlers.Release (ReleaseHandlers (..))
import Hix.Managed.Release.Data.Staged (SelectedTargetView (..))
import Hix.Managed.Release.VersionFile (writeVersionFiles)
import Hix.Managed.StateFile (writeReleaseVersions)
import Hix.Monad (appContext, clientError)
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

managedDisabled :: Text
managedDisabled =
  [exon|Managed state is disabled for this project.
In order to persist release versions, set #{Color.config @Text "managed.enable = true"} in #{Color.path @Text "flake.nix"}.
You can release the current version without persistence by specifying #{Color.shellCommand @Text "--force-version"}.|]

-- | Check whether version persistence is available for all selected packages.
--
-- When managed state is disabled and there are packages without configured version files, versions cannot be tracked
-- across releases.
-- In that case, require @--force-version@ to proceed.
checkVersionPersistence :: Bool -> Bool -> StateVersionsContext -> Packages SelectedTargetView -> M ()
checkVersionPersistence managed forceVersion svContext targets =
  unless (managed || forceVersion || isJust svContext.versionFile || all hasVersionFile (nKeys targets)) do
    clientError managedDisabled
  where
    hasVersionFile name =
      fromMaybe False do
        pkg <- Map.lookup name (nGet svContext.packages)
        pure (isJust pkg.versionFile)

updateStateVersions ::
  ReleaseHandlers ->
  Bool ->
  Bool ->
  Maybe Version ->
  Packages SelectedTargetView ->
  M ()
updateStateVersions handlers managed forceVersion sharedVersion targets =
  appContext "updating the managed state to use the new versions" do
    context <- queryContext handlers.context ContextStateVersions
    checkVersionPersistence managed forceVersion context targets
    when managed do
      Log.info "Writing new versions to managed state"
      writeReleaseVersions handlers.project.stateFile (update context.state)
    wroteVersionFiles <- writeVersionFiles sharedVersion context targets
    when wroteVersionFiles do
      Log.info "Writing version files"
  where
    update =
      (#packages %~ nPad updateVersion newPackageState targets)
      .
      (#bounds %~ updateUpperBounds targets)
