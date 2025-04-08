module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import qualified Data.Set as Set
import Path (Abs, Dir, Path)
import Path.IO (copyDirRecur')
import System.IO.Error (IOError)

import Hix.Class.Map (nToMaybe)
import Hix.Data.Error (ErrorMessage (Fatal))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M, appRes)
import Hix.Data.Overrides (Override (..), Overrides)
import Hix.Data.PackageId (PackageId (..))
import Hix.Http (httpManager)
import Hix.Managed.Build.SolverPackages (solverGhc)
import Hix.Managed.Build.Target (BuilderResources (..), EnvBuilderResources (..), buildTargets)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForVersions), allHackages)
import Hix.Managed.Cabal.Data.InstalledOverrides (InstalledOverrides (..))
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.EnvContext (EnvContext (..))
import Hix.Managed.Data.EnvState (EnvState (..))
import Hix.Managed.Data.Initial (Initial (..))
import Hix.Managed.Data.ManagedPackage (ProjectPackages)
import Hix.Managed.Handlers.AvailableVersions (AvailableVersionsHandlers)
import qualified Hix.Managed.Handlers.AvailableVersions.Prod as AvailableVersions
import Hix.Managed.Handlers.Build (
  BuildHandlers (..),
  Builder (..),
  EnvBuilder (..),
  EnvBuilderContext (..),
  InitCabal (..),
  )
import qualified Hix.Managed.Handlers.Cabal.Prod as CabalHandlers
import qualified Hix.Managed.Handlers.HackageClient.Prod as HackageClient
import Hix.Managed.Handlers.Project (ProjectHandlers (..))
import Hix.Managed.Handlers.SourceHash (SourceHashHandlers)
import qualified Hix.Managed.Handlers.SourceHash.Prod as SourceHashHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import Hix.Managed.StateFile (writeInitialEnvState)
import Hix.Monad (throwM, withTempDir)

withTempProject ::
  (Path Abs Dir -> M a) ->
  M a
withTempProject use = do
  projectRoot <- appRes.root
  catch
    do
      withTempDir "managed-build" \ tmpRoot -> do
        copyDirRecur' projectRoot tmpRoot
        use tmpRoot
    \ (err :: IOError) -> throwM (Fatal (show err))

storeSolverOverrides :: Overrides -> Initial EnvState -> EnvState
storeSolverOverrides new (Initial EnvState {..}) =
  EnvState {solver = new, ..}

-- | Determine the set of package IDs that should not be treated as installed in the solver package set when Cabal
-- selects dependencies.
--
-- This is simply the set of regular overrides, which usually should use versions that deviate from what's in the
-- nixpkgs GHC set.
-- Removing them will cause Cabal to emit overrides for these packages again in mutation builds, potentially with
-- different versions depending on the build plan.
-- If this wasn't done, the mutation build would try to use the actually installed versions, which are broken (the
-- reason for them getting overridden in the solver package set in the first place).
-- Since mutation builds can only restart with revisions of the same version on failure, the build will fail, and the
-- user would have to add those to the flake overrides manually (which was the procedure until this feature was
-- introduced).
--
-- Jailbroken packages, however, do not have to be removed – mutation builds are capable of fixing those themselves.
--
-- Local overrides do not need to be handled here because they cannot exist in solver package sets.
-- The Nix logic that constructs those sets excludes any local packages, even if they are dependencies of other local
-- packages, so checking the solver set will never result in an override of this sort.
-- Instead, these are only created when no Hackage upload exists (yet), as part of a mutation build, in
-- 'Hix.Managed.Overrides.packageOverride'.
installedOverrides :: EnvState -> InstalledOverrides
installedOverrides EnvState {solver} =
  InstalledOverrides $ Set.fromList $ nToMaybe solver \ name -> \case
    Override {..} -> Just PackageId {..}
    Jailbreak -> Nothing
    Local -> Nothing

withEnvBuilder ::
  ∀ a .
  BuilderResources ->
  EnvBuilderContext ->
  (EnvBuilder -> Initial EnvState -> M a) ->
  M a
withEnvBuilder global EnvBuilderContext {initCabal, env = EnvContext {env, targets, ghc}, initialState} use = do
  (resolvedGhc, solverOverrides) <- solverGhc global env ((.solver) <$> initialState) ghc
  let state = maybe (.value) storeSolverOverrides solverOverrides initialState
  (cabal, localUnavailable) <- initCabal.run (installedOverrides state) resolvedGhc
  writeInitialEnvState global.stateFile global.root env initialState
  use EnvBuilder {buildTargets = buildTargets EnvBuilderResources {..}, ..} (Initial state)

withBuilder ::
  SourceHashHandlers ->
  StateFileHandlers ->
  AvailableVersionsHandlers ->
  BuildConfig ->
  (Builder -> M a) ->
  M a
withBuilder hackage stateFile versions buildConfig use =
  withTempProject \ root -> do
    let resources = BuilderResources {..}
    use Builder {withEnvBuilder = withEnvBuilder resources}

handlersProdResources ::
  CabalConfig ->
  M (SourceHashHandlers, AvailableVersionsHandlers)
handlersProdResources cabalConf = do
  manager <- httpManager
  sourceHash <- SourceHashHandlers.handlersProd (allHackages cabalConf)
  hackage <- HackageClient.handlersProdFor (Just manager) ForVersions cabalConf
  let versions = AvailableVersions.handlersProd hackage
  pure (sourceHash, versions)

initCabalProd :: CabalConfig -> ProjectPackages -> InitCabal
initCabalProd cabalConfig packages =
   InitCabal \ overrides -> CabalHandlers.handlersProd overrides cabalConfig False packages

handlersProd ::
  ProjectHandlers ->
  BuildConfig ->
  CabalConfig ->
  M BuildHandlers
handlersProd project buildConfig cabalConfig = do
  (sourceHash, versions) <- handlersProdResources cabalConfig
  pure BuildHandlers {
    project,
    cabal = initCabalProd cabalConfig,
    withBuilder = withBuilder sourceHash project.stateFile versions buildConfig,
    versions
  }
