module Hix.Managed.Handlers.Build.Prod where

import Control.Monad.Catch (catch)
import Control.Monad.Trans.Reader (asks)
import Exon (exon)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Path (Abs, Dir, Path, toFilePath)
import Path.IO (copyDirRecur')
import System.IO.Error (IOError)
import System.Process.Typed (
  ExitCode (ExitFailure, ExitSuccess),
  ProcessConfig,
  inherit,
  nullStream,
  proc,
  setStderr,
  setStdout,
  setWorkingDir,
  waitExitCode,
  withProcessTerm,
  )
import System.Timeout (timeout)

import Hix.Data.EnvName (EnvName)
import Hix.Data.Error (Error (Fatal))
import qualified Hix.Data.Monad
import Hix.Data.Monad (M (M))
import Hix.Data.Overrides (Overrides)
import Hix.Data.PackageId (PackageId)
import Hix.Data.PackageName (LocalPackage)
import Hix.Data.Version (Versions)
import Hix.Error (pathText)
import Hix.Hackage (latestVersionHackage, versionsHackage)
import qualified Hix.Log as Log
import Hix.Managed.Cabal.Data.Config (CabalConfig)
import qualified Hix.Managed.Data.BuildConfig
import Hix.Managed.Data.BuildConfig (BuildConfig)
import Hix.Managed.Data.EnvConfig (EnvConfig)
import qualified Hix.Managed.Data.EnvContext
import Hix.Managed.Data.EnvContext (EnvContext (EnvContext))
import Hix.Managed.Data.EnvState (EnvState)
import Hix.Managed.Data.Envs (Envs)
import Hix.Managed.Data.Initial (Initial)
import Hix.Managed.Data.StageState (BuildResult (Finished, TimedOut), BuildStatus (Failure, Success), buildUnsuccessful)
import qualified Hix.Managed.Data.StateFileConfig
import Hix.Managed.Data.StateFileConfig (StateFileConfig)
import Hix.Managed.Data.Targets (firstMTargets)
import qualified Hix.Managed.Handlers.Build
import Hix.Managed.Handlers.Build (BuildHandlers (..), BuildOutputsPrefix, Builder (Builder), EnvBuilder (EnvBuilder))
import Hix.Managed.Handlers.Cabal (CabalHandlers)
import qualified Hix.Managed.Handlers.Cabal.Prod as CabalHandlers
import Hix.Managed.Handlers.Hackage (HackageHandlers)
import qualified Hix.Managed.Handlers.Hackage.Prod as HackageHandlers
import qualified Hix.Managed.Handlers.Report.Prod as ReportHandlers
import Hix.Managed.Handlers.StateFile (StateFileHandlers)
import qualified Hix.Managed.Handlers.StateFile.Prod as StateFileHandlers
import Hix.Managed.Overrides (packageOverrides)
import Hix.Managed.Path (rootOrCwd)
import Hix.Managed.StateFile (writeBuildStateFor, writeInitialEnvState)
import Hix.Monad (throwM, tryIOM, withTempDir)

data BuilderResources =
  BuilderResources {
    hackage :: HackageHandlers,
    stateFileHandlers :: StateFileHandlers,
    envsConf :: Envs EnvConfig,
    buildOutputsPrefix :: Maybe BuildOutputsPrefix,
    root :: Path Abs Dir,
    buildConfig :: BuildConfig
  }

data EnvBuilderResources =
  EnvBuilderResources {
    global :: BuilderResources,
    context :: EnvContext
  }

withTempProject ::
  Maybe (Path Abs Dir) ->
  (Path Abs Dir -> M a) ->
  M a
withTempProject rootOverride use = do
  projectRoot <- rootOrCwd rootOverride
  catch
    do
      withTempDir "managed-build" \ tmpRoot -> do
        copyDirRecur' projectRoot tmpRoot
        use tmpRoot
    \ (err :: IOError) -> throwM (Fatal (show err))

nixProc ::
  Bool ->
  Path Abs Dir ->
  [Text] ->
  Text ->
  [Text] ->
  M (ProcessConfig () () ())
nixProc showOutput root cmd installable extra = do
  Log.debug [exon|Running nix at '#{pathText root}' with args #{show args}|]
  conf <$> M (asks (.debug))
  where
    conf debug = err debug (setWorkingDir (toFilePath root) (proc "nix" args))

    err debug
      | debug || showOutput
      = setStderr inherit
      | otherwise
      = setStderr nullStream

    args = toString <$> cmd ++ [exon|path:#{".#"}#{installable}|] : extra

buildPackage ::
  BuildConfig ->
  Path Abs Dir ->
  EnvName ->
  LocalPackage ->
  M BuildResult
buildPackage buildConf root env target = do
  debug <- M (asks (.debug))
  conf <- nixProc buildConf.buildOutput root ["-L", "build"] [exon|env.##{env}.##{target}|] []
  tryIOM (withProcessTerm (err debug conf) (limit . waitExitCode)) <&> \case
    Just ExitSuccess -> Finished Success
    Just (ExitFailure _) -> Finished Failure
    Nothing -> TimedOut
  where
    limit | Just t <- buildConf.timeout = timeout (coerce t * 1_000_000)
          | otherwise = fmap Just

    err = \case
      True -> setStdout inherit
      False -> id

buildWithState ::
  EnvBuilderResources ->
  Versions ->
  [PackageId] ->
  M (Overrides, BuildResult)
buildWithState EnvBuilderResources {global, context = context@EnvContext {env, targets}} _ overrideVersions = do
  overrides <- packageOverrides global.hackage overrideVersions
  writeBuildStateFor "current build" global.stateFileHandlers global.root context overrides
  status <- firstMTargets (Finished Success) buildUnsuccessful (buildPackage global.buildConfig global.root env) targets
  pure (overrides, status)

-- | This used to have the purpose of reading an updated GHC package db using the current managed state, but this has
-- become obsolete.
--
-- TODO Decide whether to keep this for abstraction purposes.
withEnvBuilder ::
  ∀ a .
  BuilderResources ->
  CabalHandlers ->
  EnvContext ->
  Initial EnvState ->
  (EnvBuilder -> M a) ->
  M a
withEnvBuilder global cabal context initialState use = do
  writeInitialEnvState global.stateFileHandlers global.root context initialState
  use builder
  where
    builder =
      EnvBuilder {
        cabal,
        buildWithState = buildWithState resources
      }

    resources = EnvBuilderResources {..}

withBuilder ::
  HackageHandlers ->
  StateFileHandlers ->
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  BuildConfig ->
  (Builder -> M a) ->
  M a
withBuilder hackage stateFileHandlers stateFileConf envsConf buildOutputsPrefix buildConfig use =
  withTempProject stateFileConf.projectRoot \ root -> do
    let resources = BuilderResources {..}
    use Builder {withEnvBuilder = withEnvBuilder resources}

handlersProd ::
  MonadIO m =>
  StateFileConfig ->
  Envs EnvConfig ->
  Maybe BuildOutputsPrefix ->
  BuildConfig ->
  CabalConfig ->
  Bool ->
  m BuildHandlers
handlersProd stateFileConf envsConf buildOutputsPrefix buildConfig cabalConf oldest = do
  manager <- liftIO (newManager tlsManagerSettings)
  hackage <- HackageHandlers.handlersProd
  let stateFile = StateFileHandlers.handlersProd stateFileConf
  pure BuildHandlers {
    stateFile,
    report = ReportHandlers.handlersProd,
    cabal = CabalHandlers.handlersProd cabalConf oldest,
    withBuilder = withBuilder hackage stateFile stateFileConf envsConf buildOutputsPrefix buildConfig,
    versions = versionsHackage manager,
    latestVersion = latestVersionHackage manager
  }
