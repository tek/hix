module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import Path (Abs, Dir, Path)
import Path.IO (copyDirRecur')
import System.IO.Error (IOError)

import Hix.Data.Error (ErrorMessage (Fatal))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M, appRes)
import Hix.Data.Overrides (Overrides)
import Hix.Http (httpManager)
import Hix.Managed.Build.SolverPackages (solverGhc)
import Hix.Managed.Build.Target (BuilderResources (..), EnvBuilderResources (..), buildTargets)
import Hix.Managed.Cabal.Data.Config (CabalConfig, HackagePurpose (ForVersions), allHackages)
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

storeSolverOverrides :: Overrides -> Initial EnvState -> Initial EnvState
storeSolverOverrides new (Initial EnvState {..}) =
  Initial EnvState {solver = new, ..}

withEnvBuilder ::
  ∀ a .
  BuilderResources ->
  EnvBuilderContext ->
  (EnvBuilder -> M a) ->
  M a
withEnvBuilder global EnvBuilderContext {initCabal, env = EnvContext {env, targets, ghc}, initialState} use = do
  (resolvedGhc, solverOverrides) <- solverGhc global env ((.solver) <$> initialState) ghc
  let initialStateWithSolver = maybe id storeSolverOverrides solverOverrides initialState
  (cabal, localUnavailable) <- initCabal.run resolvedGhc
  writeInitialEnvState global.stateFile global.root env initialStateWithSolver
  use EnvBuilder {buildTargets = buildTargets EnvBuilderResources {..}, ..}

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
   InitCabal (CabalHandlers.handlersProd cabalConfig False packages)

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
